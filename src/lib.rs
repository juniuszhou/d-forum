#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
use serde_derive::{Deserialize, Serialize};

use codec::{Codec, Decode, Encode};
use frame_support::storage::migration::{put_storage_value, StorageIterator};
use frame_support::{
    decl_error, decl_event, decl_module, decl_storage,
    dispatch::DispatchResult,
    traits::{Currency, Get, LockableCurrency, ReservableCurrency},
    Parameter,
};
use rstd::prelude::*;
use sp_runtime;
use sp_runtime::traits::{AtLeast32Bit, MaybeSerialize, Member, One, Saturating, Zero};
use system;
use system::ensure_signed;

#[cfg(test)]
mod mock;
#[cfg(test)]
mod tests;

mod migration;

/// Forum data storage version
#[derive(Encode, Decode, Clone, Copy, PartialEq, Eq)]
enum Releases {
    V1_0_0,
    V2_0_0,
}

impl Default for Releases {
    fn default() -> Self {
        Releases::V1_0_0
    }
}

type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

pub trait Trait: system::Trait + timestamp::Trait + Sized {
    type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
    type ForumUserId: Parameter
        + Member
        + AtLeast32Bit
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type ModeratorId: Parameter
        + Member
        + AtLeast32Bit
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type CategoryId: Parameter
        + Member
        + AtLeast32Bit
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type ThreadId: Parameter
        + Member
        + AtLeast32Bit
        + Codec
        + Default
        + Copy
        + MaybeSerialize
        + PartialEq
        + From<u64>
        + Into<u64>;

    type PostId: Parameter
        + Member
        + AtLeast32Bit
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

/// Moderation action structure in old version.
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct OldModerationAction<AccountId, BlockNumber, Moment> {
    /// When action occured.
    pub moderated_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Account forum sudo which acted.
    pub moderator_account: AccountId,

    /// Moderation rationale
    pub rationale: Vec<u8>,
}

/// Post data structure in old version
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct OldPost<AccountId, BlockNumber, Moment> {
    /// Id of thread to which this post corresponds.
    pub thread_id: u64,

    /// Current text of post
    pub text: Vec<u8>,

    /// Possible moderation of this post
    pub moderation: Option<OldModerationAction<AccountId, BlockNumber, Moment>>,

    /// When post was submitted.
    pub created_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Author of post.
    pub author_id: AccountId,

    /// Post's position in thread
    pub index_in_thread: u32,
}

/// Thread data structure in old vesion
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct OldThread<AccountId, BlockNumber, Moment> {
    /// Title
    pub title: Vec<u8>,

    /// Category in which this thread lives
    pub category_id: u64,

    /// Possible moderation of this thread
    pub moderation: Option<OldModerationAction<AccountId, BlockNumber, Moment>>,

    /// When thread was established.
    pub created_at: BlockchainTimestamp<BlockNumber, Moment>,

    /// Author of post.
    pub author_id: AccountId,

    /// Position in category.
    pub index_in_category: u32,
}

/// Category data structure in old version
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct OldCategory<BlockNumber, Moment> {
    /// Category identifier
    pub parent_id: Option<u64>,

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

/// Represents a user's information in this forum, introduced in new version
#[cfg_attr(feature = "std", derive(Serialize, Deserialize, Debug))]
#[derive(Encode, Decode, Default, Clone, PartialEq, Eq)]
pub struct ForumUser<AccountId> {
    pub role_account: AccountId,
}

/// Represents a moderator in this forum, introduced in new version
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
    pub text: Vec<u8>,

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
    trait Store for Module<T: Trait> as Forum {
        /// Map forum user identifier to forum user information.
        pub ForumUserById get(forum_user_by_id) config(): map hasher(blake2_256) T::ForumUserId  => ForumUser<T::AccountId>;

        /// Forum user identifier value for next new forum user.
        pub NextForumUserId get(next_forum_user_id) config(): T::ForumUserId;

        /// Map forum moderator identifier to moderator information.
        pub ModeratorById get(moderator_by_id) config(): map hasher(blake2_256) T::ModeratorId => Moderator<T::AccountId>;

        /// Forum moderator identifier value for next new moderator user.
        pub NextModeratorId get(next_moderator_id) config(): T::ModeratorId;

        /// Map category identifier to corresponding category.
        pub CategoryById get(category_by_id) config(): map hasher(blake2_256) T::CategoryId => Category<T::CategoryId, T::BlockNumber, T::Moment>;

        /// Category identifier value to be used for the next Category created.
        pub NextCategoryId get(next_category_id) config(): T::CategoryId;

        /// All sub categories under a category
        pub SubcategoriesById get(sub_categories_by_id) config(): map hasher(blake2_256) T::CategoryId => Vec<T::CategoryId>;

        /// All threads under a category
        pub DirectThreadsById get(direct_threads_by_id) config(): map hasher(blake2_256) T::CategoryId => Vec<T::ThreadId>;

        /// Map thread identifier to corresponding thread.
        pub ThreadById get(thread_by_id) config(): map hasher(blake2_256) T::ThreadId => Thread<T::ForumUserId, T::ModeratorId, T::CategoryId, T::BlockNumber, T::Moment>;

        /// Thread identifier value to be used for next Thread in threadById.
        pub NextThreadId get(next_thread_id) config(): T::ThreadId;

        /// All posts under a thread
        pub PostsByThreadId get(posts_by_thread_id) config(): map hasher(blake2_256) T::ThreadId => Vec<T::PostId>;

        /// Map post identifier to corresponding post.
        pub PostById get(post_by_id) config(): map hasher(blake2_256) T::PostId => Post<T::ForumUserId, T::ModeratorId, T::ThreadId, T::BlockNumber, T::Moment>;

        /// Post identifier value to be used for for next post created.
        pub NextPostId get(next_post_id) config(): T::PostId;

        /// Account of forum sudo.
        pub ForumSudo get(forum_sudo) config(): Option<T::AccountId>;

        /// Moderator set for each Category
        pub CategoryByModerator get(category_by_moderator) config(): double_map hasher(blake2_256) T::CategoryId, hasher(blake2_256) T::ModeratorId => bool;

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

        /// Storage version of forum pallet
        StorageVersion build(|_: &GenesisConfig<T>| Releases::V2_0_0): Releases;
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

        /// Migration all data during runtime upgrade
        fn on_runtime_upgrade() {
            // Migrate all categories, threads and posts
            let (forum_users, moderators) = migration::on_runtime_upgrade::<T>();
            // Create forum users and moderators according data extract from old threads and posts
            for (forum_user_account, forum_user_id) in forum_users.iter() {
                <ForumUserById<T>>::insert(forum_user_id, ForumUser::<T::AccountId> {
                    role_account: forum_user_account.clone(),
                });
            }

            for (moderator_account, moderator_id) in moderators.iter() {
                <ModeratorById<T>>::insert(moderator_id, Moderator::<T::AccountId> {
                    role_account: moderator_account.clone(),
                });
            }

            <NextForumUserId<T>>::set((forum_users.len() as u64).into());
            <NextModeratorId<T>>::set((moderators.len() as u64).into());
        }

        /// Set forum sudo.
        fn set_forum_sudo(origin, new_forum_sudo: Option<T::AccountId>) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Not signed by forum SUDO
            Self::ensure_is_forum_sudo(&who)?;

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
        // TODO archive recursively
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
                // Set category and all child categories archived
                Self::set_category_archived_recursive(&category_id);

                // Generate event
                Self::deposit_event(RawEvent::CategoryArchieved(category_id));

                Ok(())
            }
        }

        /// Delete will return reserved fee except the thread or post moderated
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

         /// Settle a category after archived or deleted
        fn unreserve_fee_category(origin, category_id: T::CategoryId,) -> DispatchResult {
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

            // Get fee for thread
            let title_deposit_fee = T::DepositPerThread::get().saturating_add(
                <BalanceOf<T>>::from(text.len() as u32).saturating_mul(T::DepositTitlePerByte::get()),
            );

            // Get fee for post
            let text_deposit_fee = T::DepositPerPost::get().saturating_add(
                <BalanceOf<T>>::from(text.len() as u32).saturating_mul(T::DepositTextPerByte::get()),
            );

            // Reserve fee for thread and post
            <T as Trait>::Currency::reserve(&who, title_deposit_fee.saturating_add(text_deposit_fee))?;

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

            // Get fee for post
            let text_deposit_fee = T::DepositPerPost::get().saturating_add(
                <BalanceOf<T>>::from(text.len() as u32).saturating_mul(T::DepositTextPerByte::get()),
            );

            // Reserve fee for thread and post
            <T as Trait>::Currency::unreserve(&who, text_deposit_fee);

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
    // Set category and all child categories are archived
    pub fn set_category_archived_recursive(category_id: &T::CategoryId) {
        let category = <CategoryById<T>>::get(category_id);
        if !category.deleted && !category.archived {
            // Get all threads in the category
            let thread_ids = <DirectThreadsById<T>>::get(category_id);

            // Unreserve the currency for each thead and post if not moderated
            thread_ids.iter().for_each(|thread_id| {
                let thread = <ThreadById<T>>::get(thread_id);
                if thread.moderation.is_none() {
                    let fee = T::DepositPerThread::get().saturating_add(
                        <BalanceOf<T>>::from(thread.title.len() as u32)
                            .saturating_mul(T::DepositTitlePerByte::get()),
                    );
                    // Reserve fee for thread and post
                    let thread_author_account = <ForumUserById<T>>::get(&thread.author_id);
                    <T as Trait>::Currency::unreserve(&thread_author_account.role_account, fee);
                }

                let post_ids = <PostsByThreadId<T>>::get(thread_id);
                post_ids
                    .iter()
                    .map(|post_id| <PostById<T>>::get(post_id))
                    .filter(|post| post.moderation.is_none())
                    .for_each(|post| {
                        let post_author_account = <ForumUserById<T>>::get(post.author_id);
                        // Get fee for post
                        let text_deposit_fee = T::DepositPerPost::get().saturating_add(
                            <BalanceOf<T>>::from(post.text.len() as u32)
                                .saturating_mul(T::DepositTextPerByte::get()),
                        );

                        // Unreserve fee for post
                        <T as Trait>::Currency::unreserve(
                            &post_author_account.role_account,
                            text_deposit_fee,
                        );
                    })
            });

            // Apply the same operation to all child categories
            let child_categories = <SubcategoriesById<T>>::get(category_id);
            <CategoryById<T>>::mutate(category_id, |value| value.archived = true);
            child_categories
                .iter()
                .for_each(|id| Self::set_category_archived_recursive(id));
        }
    }

    // Set category and all child categories are deleted
    pub fn set_category_deleted_recursive(category_id: &T::CategoryId) {
        let category = <CategoryById<T>>::get(category_id);
        if !category.deleted {
            let child_categories = <SubcategoriesById<T>>::get(category_id);
            <CategoryById<T>>::mutate(category_id, |value| value.deleted = true);
            child_categories
                .iter()
                .for_each(|id| Self::set_category_archived_recursive(id));
        }
    }

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

        // Create and add new thread
        let new_thread_id = <NextThreadId<T>>::get();

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

        // Add inital post to thread
        Self::add_new_post(new_thread_id, &text, author_id)?;

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

        // Make and add initial post
        let new_post_id = <NextPostId<T>>::get();

        // Get post amount in thread
        let post_amount = <PostsByThreadId<T>>::get(&thread_id).len();

        // Build a post
        let new_post = Post {
            thread_id: thread_id,
            text: text.clone(),
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
        let thread = Self::ensure_thread_exists(&thread_id)?;

        if thread.moderation.is_some() {
            Err(Error::<T>::ThreadAlreadyModerated.into())
        } else {
            Self::ensure_category_mutable(Some(thread.category_id))
        }
    }

    fn ensure_forum_sudo_set() -> DispatchResult {
        match <ForumSudo<T>>::get() {
            Some(_) => Ok(()),
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
        if <ForumUserById<T>>::contains_key(forum_user_id) {
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
        if <ModeratorById<T>>::contains_key(moderator_id) {
            if <ModeratorById<T>>::get(moderator_id).role_account == *account_id {
                Ok(())
            } else {
                Err(Error::<T>::ModeratorIdNotMatchAccount.into())
            }
        } else {
            Err(Error::<T>::OriginNotModerator.into())
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
        if <PostById<T>>::contains_key(post_id) {
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
        if <ThreadById<T>>::contains_key(thread_id) {
            Ok(<ThreadById<T>>::get(thread_id))
        } else {
            Err(Error::<T>::ThreadNotExisted.into())
        }
    }

    fn ensure_category_exists(
        category_id: &T::CategoryId,
    ) -> Result<Category<T::CategoryId, T::BlockNumber, T::Moment>, &'static str> {
        if <CategoryById<T>>::contains_key(category_id) {
            Ok(<CategoryById<T>>::get(category_id))
        } else {
            Err(Error::<T>::CategoryNotExisted.into())
        }
    }
}
