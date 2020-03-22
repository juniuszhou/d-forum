use super::*;
use rstd::collections::btree_map::BTreeMap;

// Triggered when runtime upgrade
pub fn on_runtime_upgrade<T: Trait>() -> (
    BTreeMap<T::AccountId, T::ForumUserId>,
    BTreeMap<T::AccountId, T::ModeratorId>,
) {
    match StorageVersion::get() {
        // Nothing if already upgraded
        Releases::V2_0_0 => (BTreeMap::new(), BTreeMap::new()),
        // Start the upgrade process
        Releases::V1_0_0 => upgrade_v1_to_v2::<T>(),
    }
}

// Upgrade from V1 to V2. There are some changes between V1 and V2
// The CategoryId ThreadId and PostId defined as u64 in V1
// In V2, type definition get from Trait
// The ForumUserId and ModeratorId are introduced in V2.
fn upgrade_v1_to_v2<T: Trait>() -> (
    BTreeMap<T::AccountId, T::ForumUserId>,
    BTreeMap<T::AccountId, T::ModeratorId>,
) {
    for (hash, category) in
        StorageIterator::<OldCategory<T::BlockNumber, T::Moment>>::new(b"Forum", b"CategoryById")
            .drain()
    {
        // Migrate all categories
        let new_category = Category::<T::CategoryId, T::BlockNumber, T::Moment> {
            parent_id: match category.parent_id {
                Some(id) => Some(id.into()),
                None => None,
            },
            title: category.title,
            description: category.description,
            created_at: category.created_at,
            deleted: category.deleted,
            archived: category.archived,
            index_in_parent_category: category.index_in_parent_category,
        };

        // Save upgraded category into storage
        put_storage_value(b"Balances", b"CategoryById", &hash, new_category);
    }

    // Keep all forum user and moderator appearred in old thread and post
    let mut next_forum_id: T::ForumUserId = Zero::zero();
    let mut next_moderator_id: T::ModeratorId = Zero::zero();
    let mut account_to_forum_user = BTreeMap::new();
    let mut account_to_moderator = BTreeMap::new();

    // Migrate all threads
    for (hash, thread) in
        StorageIterator::<OldThread<T::AccountId, T::BlockNumber, T::Moment>>::new(
            b"Forum",
            b"ThreadById",
        )
        .drain()
    {
        // Insert new appearred forum user
        if !account_to_forum_user.contains_key(&thread.author_id) {
            account_to_forum_user.insert(thread.author_id.clone(), next_forum_id);
            next_forum_id += One::one();
        }
        // Upgrade moderation data
        let new_moderation = match thread.moderation {
            Some(moderation) => {
                // If an account not registered as moderator
                if !account_to_moderator.contains_key(&moderation.moderator_account) {
                    account_to_moderator
                        .insert(moderation.moderator_account.clone(), next_moderator_id);
                    next_moderator_id += One::one();
                }
                // Build a moderation
                Some(
                    ModerationAction::<T::ModeratorId, T::BlockNumber, T::Moment> {
                        moderated_at: moderation.moderated_at,
                        moderator_id: *account_to_moderator
                            .get(&moderation.moderator_account)
                            .unwrap(),
                        rationale: moderation.rationale,
                    },
                )
            }
            None => None,
        };

        // Build new thread
        let new_thread =
            Thread::<T::ForumUserId, T::ModeratorId, T::CategoryId, T::BlockNumber, T::Moment> {
                title: thread.title,
                category_id: thread.category_id.into(),
                moderation: new_moderation,
                created_at: thread.created_at,
                author_id: *account_to_forum_user.get(&thread.author_id).unwrap(),
                index_in_category: thread.index_in_category,
            };

        // Put new thread into storage
        put_storage_value(b"Forum", b"ThreadById", &hash, new_thread);
    }

    // Migrate all post
    for (hash, post) in StorageIterator::<OldPost<T::AccountId, T::BlockNumber, T::Moment>>::new(
        b"Forum",
        b"ThreadById",
    )
    .drain()
    {
        // Insert new appearred forum user
        if !account_to_forum_user.contains_key(&post.author_id) {
            account_to_forum_user.insert(post.author_id.clone(), next_forum_id);
            next_forum_id += One::one();
        }
        // Update the moderation
        let new_moderation = match post.moderation {
            Some(moderation) => {
                // If an account not registered as moderator
                if !account_to_moderator.contains_key(&moderation.moderator_account) {
                    account_to_moderator
                        .insert(moderation.moderator_account.clone(), next_moderator_id);
                    next_moderator_id += One::one();
                }
                // Build a moderation
                Some(
                    ModerationAction::<T::ModeratorId, T::BlockNumber, T::Moment> {
                        moderated_at: moderation.moderated_at,
                        moderator_id: *account_to_moderator
                            .get(&moderation.moderator_account)
                            .unwrap(),
                        rationale: moderation.rationale,
                    },
                )
            }
            None => None,
        };

        // Build the new post
        let new_post =
            Post::<T::ForumUserId, T::ModeratorId, T::ThreadId, T::BlockNumber, T::Moment> {
                thread_id: post.thread_id.into(),
                text: post.text,
                moderation: new_moderation,
                created_at: post.created_at,
                author_id: *account_to_forum_user.get(&post.author_id).unwrap(),
                index_in_thread: post.index_in_thread,
            };

        // Store the new post
        put_storage_value(b"Forum", b"PostById", &hash, new_post);
    }

    // Storage migration is done
    StorageVersion::put(Releases::V2_0_0);

    // Return all forum user and moderator accounts
    (account_to_forum_user, account_to_moderator)
}
