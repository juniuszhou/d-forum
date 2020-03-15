#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
use serde_derive::{Deserialize, Serialize};

use codec::{Codec, Decode, Encode};
use frame_support::{
    decl_error, decl_event, decl_module, decl_storage,
    dispatch::{DispatchError, DispatchResult},
    ensure,
    traits::{Currency, Get, LockableCurrency, ReservableCurrency},
    Parameter,
};
use rstd::prelude::*;
use sp_runtime;
use sp_runtime::traits::{MaybeSerialize, Member, One, Saturating, SimpleArithmetic};

// mod mock;
// mod tests;

type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

pub trait Trait: system::Trait + timestamp::Trait + Sized {
    type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
    type ForumUserId: Parameter
        + Member
        + SimpleArithmetic
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq;

    type ModeratorId: Parameter
        + Member
        + SimpleArithmetic
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq;

    type CategoryId: Parameter
        + Member
        + SimpleArithmetic
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type ThreadId: Parameter
        + Member
        + SimpleArithmetic
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type PostId: Parameter
        + Member
        + SimpleArithmetic
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    // Fee for create a thread
    type DepositPerThread: Get<BalanceOf<Self>>;

    // Fee for create a post
    type DepositPerPost: Get<BalanceOf<Self>>;

    // Fee for store one byte in title
    type DepositTitlePerByte: Get<BalanceOf<Self>>;

    // Fee for store one byte in text
    type DepositTextPerByte: Get<BalanceOf<Self>>;

    // Currency type for this module.
    type Currency: ReservableCurrency<Self::AccountId>
        + LockableCurrency<Self::AccountId, Moment = Self::BlockNumber>;
}

/// Length constraint for input validation
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct InputLengthConstraint {
    /// Minimum length
    pub min: u16,

    /// Maximum length
    pub max: u16,
}

impl InputLengthConstraint {
    pub fn length_valid(&self, len: u16) -> bool {
        len > self.min && len < self.max
    }
}

decl_error! {
    pub enum Error for Module<T: Trait> {
        InvalidTextLength,
        ForumSudoNotSet,
        OriginNotFromSudo,
        OriginNotForumUser,
        OriginNotModerator,
        ForumUserIdNotMatchAccount,
        ModeratorIdNotMatchAccount,
        PostNotExisted,
        PostAlreadyModerated,
        ThreadNotExisted,
        ThreadAlreadyModerated,
        CategoryNotExisted,
        CategoryImmutable,
        CategoryAlreadyDeteled,
        CategoryAlreadyArchived,
        CategoryCanNotModerate,
    }
}

use system;
use system::{ensure_root, ensure_signed};

/// Represents a user's information in this forum.
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct ForumUser<AccountId> {
    pub role_account: AccountId,
}

/// Represents a moderator in this forum.
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct Moderator<AccountId> {
    /// Moderator's account used for extrinsic
    pub role_account: AccountId,
}

/// Convenient composite time stamp
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct BlockchainTimestamp<BlockNumber, Moment> {
    /// Current block number
    pub block: BlockNumber,

    /// Time of block created
    pub time: Moment,
}

/// Represents a moderation outcome applied to a post or a thread.
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct ModerationAction<ModeratorId, BlockNumber, Moment> {
    /// When action occured.
    pub moderated_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Account forum sudo which acted.
    pub moderator_id: ModeratorId,

    /// Moderation rationale
    pub rationale: Vec<u8>,
}

/// Represents a thread post
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct Post<ForumUserId, ModeratorId, ThreadId, BlockNumber, Moment> {
    /// Id of thread to which this post corresponds.
    pub thread_id: ThreadId,

    /// Current text of post
    pub current_text: Vec<u8>,

    /// Possible moderation of this post
    pub moderation: Option<ModerationAction<ModeratorId, BlockNumber, Moment>>,

    /// When post was submitted.
    pub created_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Author of post.
    pub author_id: ForumUserId,

    /// Post's position in thread
    pub index_in_thread: u32,
}

/// Represents a thread
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct Thread<ForumUserId, ModeratorId, CategoryId, BlockNumber, Moment> {
    /// Title
    pub title: Vec<u8>,

    /// Category in which this thread lives
    pub category_id: CategoryId,

    /// Possible moderation of this thread
    pub moderation: Option<ModerationAction<ModeratorId, BlockNumber, Moment>>,

    /// When thread was established.
    pub created_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Author of post.
    pub author_id: ForumUserId,

    /// Position in category.
    pub index_in_category: u32,
}

/// Represents a category
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct Category<CategoryId, BlockNumber, Moment> {
    /// Category identifier
    pub parent_id: Option<CategoryId>,

    /// Title
    pub title: Vec<u8>,

    /// Description
    pub description: Vec<u8>,

    /// When category was established.
    pub created_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Whether category is deleted.
    pub deleted: bool,

    /// Whether category is archived.
    pub archived: bool,

    /// Position in parent category.
    pub index_in_parent_category: u32,
}

decl_storage! {
    trait Store for Module<T: Trait> as Forum_1_1 {
        /// Map forum user identifier to forum user information.
        pub ForumUserById get(forum_user_by_id) config(): map T::ForumUserId  => ForumUser<T::AccountId>;

        /// Forum user identifier value for next new forum user.
        pub NextForumUserId get(next_forum_user_id) config(): T::ForumUserId;

        /// Map forum moderator identifier to moderator information.
        pub ModeratorById get(moderator_by_id) config(): map T::ModeratorId => Moderator<T::AccountId>;

        /// Forum moderator identifier value for next new moderator user.
        pub NextModeratorId get(next_moderator_id) config(): T::ModeratorId;

        /// Map category identifier to corresponding category.
        pub CategoryById get(category_by_id) config(): map T::CategoryId => Category<T::CategoryId, T::BlockNumber, T::Moment>;

        /// Category identifier value to be used for the next Category created.
        pub NextCategoryId get(next_category_id) config(): T::CategoryId;

        /// All sub categories under a category
        pub SubcategoriesById get(sub_categories_by_id) config(): map T::CategoryId => Vec<T::CategoryId>;

        /// All threads under a category
        pub DirectThreadsById get(direct_threads_by_id) config(): map T::CategoryId => Vec<T::ThreadId>;

        /// Map thread identifier to corresponding thread.
        pub ThreadById get(thread_by_id) config(): map T::ThreadId => Thread<T::ForumUserId, T::ModeratorId, T::CategoryId, T::BlockNumber, T::Moment>;

        /// Thread identifier value to be used for next Thread in threadById.
        pub NextThreadId get(next_thread_id) config(): T::ThreadId;

        /// All posts under a thread
        pub PostsByThreadId get(posts_by_thread_id) config(): map T::ThreadId => Vec<T::PostId>;

        /// Map post identifier to corresponding post.
        pub PostById get(post_by_id) config(): map T::PostId => Post<T::ForumUserId, T::ModeratorId, T::ThreadId, T::BlockNumber, T::Moment>;

        /// Post identifier value to be used for for next post created.
        pub NextPostId get(next_post_id) config(): T::PostId;

        /// Account of forum sudo.
        pub ForumSudo get(forum_sudo) config(): Option<T::AccountId>;

        /// Moderator set for each Category
        pub CategoryByModerator get(category_by_moderator) config(): double_map T::CategoryId, blake2_256(T::ModeratorId) => bool;

        /// Input constraints for description text of category title.
        pub CategoryTitleConstraint get(category_title_constraint) config(): InputLengthConstraint;

        /// Input constraints for description text of category description.
        pub CategoryDescriptionConstraint get(category_description_constraint) config(): InputLengthConstraint;

        /// Input constraints for description text of thread title.
        pub ThreadTitleConstraint get(thread_title_constraint) config(): InputLengthConstraint;

        /// Input constraints for description text of post.
        pub PostTextConstraint get(post_text_constraint) config(): InputLengthConstraint;

        /// Input constraints for description text of moderation thread rationale.
        pub ModerationRationaleConstraint get(moderation_rationale_constraint) config(): InputLengthConstraint;
    }
}

decl_event!(
    pub enum Event<T>
    where
        <T as system::Trait>::AccountId,
        <T as Trait>::CategoryId,
        <T as Trait>::ThreadId, 
        <T as Trait>::PostId,
        <T as Trait>::ForumUserId,
        <T as Trait>::ModeratorId,
    {
        /// A category was introduced
        CategoryCreated(CategoryId),

        /// A category was archieved
        CategoryArchieved(CategoryId),

        /// A category was deleted
        CategoryDeleted(CategoryId),

        /// A thread with given id was created.
        ThreadCreated(ThreadId),

        /// A thread with given id was moderated.
        ThreadModerated(ThreadId),

        /// Post with given id was created.
        PostAdded(PostId),

        /// Post with givne id was moderated.
        PostModerated(PostId),

        /// Given account was set as forum sudo.
        ForumSudoSet(Option<AccountId>, Option<AccountId>),

        /// Forum user created
        ForumUserCreated(ForumUserId),

        /// Moderator created
        ModeratorCreated(ModeratorId),
    }
);

decl_module! {
    pub struct Module<T: Trait> for enum Call where origin: T::Origin {
        fn deposit_event() = default;
        // Fee for create a thread
        const DepositPerThread: BalanceOf<T> = T::DepositPerThread::get();

        // Fee for create a post
        const DepositPerPost: BalanceOf<T> = T::DepositPerPost::get();

        // Fee for store one byte in title
        const DepositTitlePerByte: BalanceOf<T> = T::DepositTitlePerByte::get();

        // Fee for store one byte in text
        const DepositTextPerByte: BalanceOf<T> = T::DepositTextPerByte::get();

        /// Set forum sudo.
        fn set_forum_sudo(origin, new_forum_sudo: Option<T::AccountId>) -> DispatchResult {
            ensure_root(origin)?;

            // Hold on to old value
            let old_forum_sudo = <ForumSudo<T>>::get().clone();

            // Update forum sudo
            match new_forum_sudo.clone() {
                Some(account_id) => <ForumSudo<T>>::mutate(|value| *value = Some(account_id)),
                None => <ForumSudo<T>>::kill()
            };

            // Generate event
            Self::deposit_event(RawEvent::ForumSudoSet(old_forum_sudo, new_forum_sudo));

            Ok(())
        }

        /// Set if a moderator can moderate a category and its sub categories.
        fn set_moderator_category(origin, moderator_id: T::ModeratorId, category_id: T::CategoryId, new_value: bool) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Not signed by forum SUDO
            Self::ensure_is_forum_sudo(&who)?;

            // ensure category exists.
            Self::ensure_category_exists(&category_id)?;

            // Get moderator id.
            Self::ensure_is_moderator_with_correct_account(&who, &moderator_id)?;

            // Put moderator into category by moderator map
            <CategoryByModerator<T>>::mutate(category_id, moderator_id, |value|
                *value = new_value);

            Ok(())
        }

        /// Add a new category.
        fn create_category(origin, parent: Option<T::CategoryId>, title: Vec<u8>, description: Vec<u8>) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Not signed by forum SUDO
            Self::ensure_is_forum_sudo(&who)?;

            // Validate title
            Self::ensure_category_title_is_valid(&title)?;

            // Validate description
            Self::ensure_category_description_is_valid(&description)?;

            // Ensure category path is mutable
            Self::ensure_category_mutable(parent)?;

            let mut child_category_len = 0;

            // If not root, then check that we can create in parent category
            if let Some(parent_category_id) = parent {
                // Get number of child category
                child_category_len = <SubcategoriesById<T>>::get(parent_category_id).len();
            }

            // Get next category id
            let next_category_id = <NextCategoryId<T>>::get();

            // Create new category
            let new_category = Category {
                parent_id : parent,
                title : title.clone(),
                description: description.clone(),
                created_at : Self::current_block_and_time(),
                deleted: false,
                archived: false,
                index_in_parent_category: (child_category_len + 1) as u32,
            };

            // Insert category in map
            <CategoryById<T>>::mutate(next_category_id, |value| *value = new_category);

            // Update other next category id
            <NextCategoryId<T>>::mutate(|value| *value += One::one());

            // Update child category list
            if let Some(parent_category_id) = parent {
                <SubcategoriesById<T>>::mutate(parent_category_id, |value| value.push(next_category_id));
            }

            // Generate event
            Self::deposit_event(RawEvent::CategoryCreated(next_category_id));

            Ok(())
        }

        /// Set category archived, then no new thread and post
        fn set_category_archived(origin, category_id: T::CategoryId,) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Not signed by forum SUDO
            Self::ensure_is_forum_sudo(&who)?;

            // Make sure category existed.
            Self::ensure_category_exists(&category_id)?;

            // Get the category
            let category = <CategoryById<T>>::get(category_id);

            if category.deleted {
                Err(Error::<T>::CategoryAlreadyDeteled.into())
            } else if category.archived {
                Err(Error::<T>::CategoryAlreadyArchived.into())
            } else {
                // Mutate category, and set possible new change parameters
                <CategoryById<T>>::mutate(category_id, |c| {
                        c.archived = true;
                });

                // Generate event
                Self::deposit_event(RawEvent::CategoryArchieved(category_id));

                Ok(())
            }
        }

        /// Update category
        fn set_category_deleted(origin, category_id: T::CategoryId,) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Not signed by forum SUDO
            Self::ensure_is_forum_sudo(&who)?;

            // Make sure category existed.
            Self::ensure_category_exists(&category_id)?;

            // Get the category
            let category = <CategoryById<T>>::get(category_id);

            if category.deleted {
                Err(Error::<T>::CategoryAlreadyDeteled.into())
            } else {
                // Mutate category, and set possible new change parameters
                <CategoryById<T>>::mutate(category_id, |c| {
                        c.deleted = true;
                });
    
                // Generate event
                Self::deposit_event(RawEvent::CategoryDeleted(category_id,));
    
                Ok(())
            }
        }

        /// Create new thread in category
        fn create_thread(origin, forum_user_id: T::ForumUserId, category_id: T::CategoryId, title: Vec<u8>, text: Vec<u8>,
        ) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Check that account is forum member
            Self::ensure_is_forum_member_with_correct_account(&who, &forum_user_id)?;

            // Keep next thread id
            let next_thread_id = <NextThreadId<T>>::get();

            // Create a new thread
            Self::add_new_thread(category_id, forum_user_id, &title, &text)?;

            // Generate event
            Self::deposit_event(RawEvent::ThreadCreated(next_thread_id));

            Ok(())
        }

        /// Moderate thread
        fn moderate_thread(origin, moderator_id: T::ModeratorId, thread_id: T::ThreadId, rationale: Vec<u8>) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Ensure origin is medorator
            Self::ensure_is_moderator_with_correct_account(&who, &moderator_id)?;

            // Get thread
            let mut thread = Self::ensure_thread_exists(&thread_id)?;

            // Rationale valid
            Self::ensure_moderation_rationale_is_valid(&rationale)?;

            // ensure origin can moderate category
            Self::ensure_moderate_category(&moderator_id, thread.category_id)?;

            // Add moderation to thread
            thread.moderation = Some(ModerationAction {
                moderated_at: Self::current_block_and_time(),
                moderator_id: moderator_id,
                rationale: rationale.clone()
            });

            // Insert new value into map
            <ThreadById<T>>::mutate(thread_id, |value| *value = thread.clone());

            // Generate event
            Self::deposit_event(RawEvent::ThreadModerated(thread_id));

            Ok(())
        }

        /// Edit post text
        fn add_post(origin, forum_user_id: T::ForumUserId, thread_id: T::ThreadId, text: Vec<u8>) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Check that account is forum member
            Self::ensure_is_forum_member_with_correct_account(&who, &forum_user_id)?;

            // Keep next post id
            let next_post_id = <NextPostId<T>>::get();

            // Add new post
            Self::add_new_post(thread_id, &text, forum_user_id)?;

            // Generate event
            Self::deposit_event(RawEvent::PostAdded(next_post_id));

            Ok(())
        }

        /// Moderate post
        fn moderate_post(origin, moderator_id: T::ModeratorId, post_id: T::PostId, rationale: Vec<u8>) -> DispatchResult {
            // Check that its a valid signature
            let who = ensure_signed(origin)?;

            // Get moderator id.
            Self::ensure_is_moderator_with_correct_account(&who, &moderator_id)?;

            // Ensure rationale text is valid
            Self::ensure_moderation_rationale_is_valid(&rationale)?;

            // Ensure post exist
            Self::ensure_post_exists(&post_id)?;

            // Get the post
            let post = <PostById<T>>::get(post_id);

            // Check if post already moderated
            if post.moderation.is_some() {
                Err(Error::<T>::PostAlreadyModerated.into())
            } else {
                // make sure origin can moderate the category
                Self::ensure_thread_is_mutable(&post.thread_id)?;
    
                // Update moderation action on post
                let moderation_action = ModerationAction{
                    moderated_at: Self::current_block_and_time(),
                    moderator_id: moderator_id,
                    rationale: rationale.clone()
                };
    
                // Update post with moderation
                <PostById<T>>::mutate(post_id, |p| p.moderation = Some(moderation_action));
    
                // Generate event
                Self::deposit_event(RawEvent::PostModerated(post_id));
    
                Ok(())
            }
        }
    }
}

impl<T: Trait> Module<T> {
    // Interface to add a new thread, could be called by other runtime
    pub fn add_new_thread(
        category_id: T::CategoryId,
        author_id: T::ForumUserId,
        title: &Vec<u8>,
        text: &Vec<u8>,
    ) -> DispatchResult {
        // Ensure category path is mutable
        Self::ensure_category_mutable(Some(category_id))?;

        // Validate title
        Self::ensure_thread_title_is_valid(&title)?;

        // Validate post text
        Self::ensure_post_text_is_valid(&text)?;

        // Compute the currency needed for thread and post

        // Create and add new thread
        let new_thread_id = <NextThreadId<T>>::get();

        // Add inital post to thread
        Self::add_new_post(new_thread_id, &text, author_id)?;

        // Get threads amount
        let child_threads_len = <DirectThreadsById<T>>::get(category_id).len();

        // Build a new thread
        let new_thread = Thread {
            title: title.clone(),
            category_id: category_id,
            moderation: None,
            created_at: Self::current_block_and_time(),
            author_id: author_id,
            index_in_category: (child_threads_len + 1) as u32,
        };

        // Store thread
        <ThreadById<T>>::mutate(new_thread_id, |value| *value = new_thread.clone());

        // Update next thread id
        <NextThreadId<T>>::mutate(|n| *n += One::one());

        // Append thread id to category
        <DirectThreadsById<T>>::mutate(category_id, |value| value.push(new_thread_id));

        Ok(())
    }

    pub fn add_new_post(
        thread_id: T::ThreadId,
        text: &Vec<u8>,
        author_id: T::ForumUserId,
    ) -> DispatchResult {
        // Validate post text
        Self::ensure_post_text_is_valid(text)?;

        // Make sure thread exists and is mutable
        Self::ensure_thread_is_mutable(&thread_id)?;

        // Get the thread
        let thread = <ThreadById<T>>::get(&thread_id);

        // Ensure category path is mutable
        Self::ensure_category_mutable(Some(thread.category_id))?;

        // Reserve fee for post
        let total_deposit = T::DepositPerPost::get().saturating_add(
            <BalanceOf<T>>::from(text.len() as u32).saturating_mul(T::DepositTextPerByte::get()),
        );

        // Make and add initial post
        let new_post_id = <NextPostId<T>>::get();

        // Get post amount in thread
        let post_amount = <PostsByThreadId<T>>::get(&thread_id).len();

        // Build a post
        let new_post = Post {
            thread_id: thread_id,
            current_text: text.clone(),
            moderation: None,
            created_at: Self::current_block_and_time(),
            author_id: author_id,
            index_in_thread: (post_amount + 1) as u32,
        };

        // Store post
        <PostById<T>>::mutate(new_post_id, |value| *value = new_post.clone());

        // Update next post id
        <NextPostId<T>>::mutate(|n| *n += One::one());

        Ok(())
    }

    // The method only called from other module to create a forum user.
    pub fn create_forum_user(account_id: T::AccountId) -> DispatchResult {
        // Create new forum user data
        let new_forum_user = ForumUser {
            role_account: account_id.clone(),
        };

        // Insert new user data for forum user
        <ForumUserById<T>>::mutate(<NextForumUserId<T>>::get(), |value| *value = new_forum_user);

        // Store event to runtime
        Self::deposit_event(RawEvent::ForumUserCreated(<NextForumUserId<T>>::get()));

        // Update forum user index
        <NextForumUserId<T>>::mutate(|n| *n += One::one());

        Ok(())
    }

    // The method only called from other module to create a new moderator.
    pub fn create_moderator(account_id: T::AccountId) -> DispatchResult {
        // Create moderator data
        let new_moderator = Moderator {
            role_account: account_id.clone(),
        };

        // Insert moderator data into storage
        <ModeratorById<T>>::mutate(<NextModeratorId<T>>::get(), |value| *value = new_moderator);

        // Store event to runtime
        Self::deposit_event(RawEvent::ModeratorCreated(<NextModeratorId<T>>::get()));

        // Update next moderate index
        <NextModeratorId<T>>::mutate(|n| *n += One::one());

        Ok(())
    }

    fn current_block_and_time() -> BlockchainTimestamp<T::BlockNumber, T::Moment> {
        BlockchainTimestamp {
            block: <system::Module<T>>::block_number(),
            time: <timestamp::Module<T>>::now(),
        }
    }

    fn ensure_thread_is_mutable(thread_id: &T::ThreadId) -> DispatchResult {
        // Make sure thread exists
        let thread = Self::ensure_thread_exists(&thread_id)?;

        if thread.moderation.is_some() {
            Err(Error::<T>::ThreadAlreadyModerated.into())
        } else {
            Self::ensure_category_mutable(Some(thread.category_id))
        }
    }

    fn ensure_forum_sudo_set() -> DispatchResult {
        match <ForumSudo<T>>::get() {
            Some(account_id) => Ok(()),
            None => Err(Error::<T>::ForumSudoNotSet.into()),
        }
    }

    fn ensure_is_forum_sudo(account_id: &T::AccountId) -> DispatchResult {
        Self::ensure_forum_sudo_set()?;
        if *account_id == <ForumSudo<T>>::get().unwrap() {
            Ok(())
        } else {
            Err(Error::<T>::OriginNotFromSudo.into())
        }
    }

    /// Ensure forum user id registered and its account id matched
    fn ensure_is_forum_member_with_correct_account(
        account_id: &T::AccountId,
        forum_user_id: &T::ForumUserId,
    ) -> DispatchResult {
        if <ForumUserById<T>>::exists(forum_user_id) {
            if <ForumUserById<T>>::get(forum_user_id).role_account == *account_id {
                Ok(())
            } else {
                Err(Error::<T>::ForumUserIdNotMatchAccount.into())
            }
        } else {
            Err(Error::<T>::OriginNotForumUser.into())
        }
    }

    /// Ensure moderator id registered and its accound id matched
    fn ensure_is_moderator_with_correct_account(
        account_id: &T::AccountId,
        moderator_id: &T::ModeratorId,
    ) -> DispatchResult {
        if <ModeratorById<T>>::exists(moderator_id) {
            if <ModeratorById<T>>::get(moderator_id).role_account == *account_id {
                Ok(())
            } else {
                Err(Error::<T>::ModeratorIdNotMatchAccount.into())
            }
        } else {
            Err(Error::<T>::OriginNotModerator.into())
        }
    }

    // Ensure thread and its category mutable
    fn ensure_thread_mutable(thread_id: T::ThreadId) -> DispatchResult {
        let thread = <ThreadById<T>>::get(thread_id);
        if thread.moderation.is_some() {
            Err(Error::<T>::ThreadAlreadyModerated.into())
        } else {
            Self::ensure_category_mutable(Some(thread.category_id))
        }
    }

    // Ensure category and its parent category mutable, means not archived or deleted.
    fn ensure_category_mutable(category_id: Option<T::CategoryId>) -> DispatchResult {
        match category_id {
            Some(id) => {
                let category = <CategoryById<T>>::get(&id);
                if category.deleted || category.archived {
                    Err(Error::<T>::CategoryImmutable.into())
                } else {
                    Self::ensure_category_mutable(category.parent_id)
                }
            }
            None => Ok(()),
        }
    }

    /// check if an account can moderate a category.
    fn ensure_moderate_category(
        moderator_id: &T::ModeratorId,
        category_id: T::CategoryId,
    ) -> DispatchResult {
        // Check if the moderator can moderate the category
        if <CategoryByModerator<T>>::get(category_id, moderator_id) {
            Ok(())
        } else {
            Err(Error::<T>::CategoryCanNotModerate.into())
        }
    }

    fn ensure_category_title_is_valid(title: &Vec<u8>) -> DispatchResult {
        if CategoryTitleConstraint::get().length_valid(title.len() as u16) {
            Ok(())
        } else {
            Err(Error::<T>::InvalidTextLength.into())
        }
    }

    fn ensure_category_description_is_valid(description: &Vec<u8>) -> DispatchResult {
        if CategoryDescriptionConstraint::get().length_valid(description.len() as u16) {
            Ok(())
        } else {
            Err(Error::<T>::InvalidTextLength.into())
        }
    }

    fn ensure_moderation_rationale_is_valid(rationale: &Vec<u8>) -> DispatchResult {
        if ModerationRationaleConstraint::get().length_valid(rationale.len() as u16) {
            Ok(())
        } else {
            Err(Error::<T>::InvalidTextLength.into())
        }
    }

    fn ensure_thread_title_is_valid(title: &Vec<u8>) -> DispatchResult {
        if ThreadTitleConstraint::get().length_valid(title.len() as u16) {
            Ok(())
        } else {
            Err(Error::<T>::InvalidTextLength.into())
        }
    }

    fn ensure_post_text_is_valid(text: &Vec<u8>) -> DispatchResult {
        if PostTextConstraint::get().length_valid(text.len() as u16) {
            Ok(())
        } else {
            Err(Error::<T>::InvalidTextLength.into())
        }
    }

    fn ensure_post_exists(
        post_id: &T::PostId,
    ) -> Result<
        Post<T::ForumUserId, T::ModeratorId, T::ThreadId, T::BlockNumber, T::Moment>,
        &'static str,
    > {
        if <PostById<T>>::exists(post_id) {
            Ok(<PostById<T>>::get(post_id))
        } else {
            Err(Error::<T>::PostNotExisted.into())
        }
    }

    fn ensure_thread_exists(
        thread_id: &T::ThreadId,
    ) -> Result<
        Thread<T::ForumUserId, T::ModeratorId, T::CategoryId, T::BlockNumber, T::Moment>,
        &'static str,
    > {
        if <ThreadById<T>>::exists(thread_id) {
            Ok(<ThreadById<T>>::get(thread_id))
        } else {
            Err(Error::<T>::ThreadNotExisted.into())
        }
    }

    fn ensure_category_exists(
        category_id: &T::CategoryId,
    ) -> Result<Category<T::CategoryId, T::BlockNumber, T::Moment>, &'static str> {
        if <CategoryById<T>>::exists(category_id) {
            Ok(<CategoryById<T>>::get(category_id))
        } else {
            Err(Error::<T>::CategoryNotExisted.into())
        }
    }
}
