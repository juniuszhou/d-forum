#![cfg(test)]

use super::*;
use crate::mock::*;

/*
 * create_forum_user
 */
#[test]
// test case for create a new forum user
fn create_forum_user_account_id() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    build_test_externalities(config).execute_with(|| {
        create_forum_user_mock(forum_sudo, Ok(()));
    });
}

#[test]
// test case for create a new moderator
fn create_moderator_account_id() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    build_test_externalities(config).execute_with(|| {
        create_moderator_mock(forum_sudo, Ok(()));
    });
}

#[test]
// test the blockchain sudo account can update forum sudo
fn set_forum_sudo() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        set_forum_sudo_mock(origin, Some(forum_sudo), Ok(()));
    });
}

#[test]
// test case for check if origin is forum sudo
fn set_moderator_category_origin() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);

    build_test_externalities(config).execute_with(|| {
        let moderator_id = create_moderator_mock(forum_sudo, Ok(()));
        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );
        set_moderator_category_mock(origin, moderator_id, category_id, true, Ok(()));
    });
}

/*
 ** update_category
 */
#[test]
// test if category updator is forum sudo
fn set_category_archived() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );

        set_category_archived_mock(origin, category_id, Ok(()));
    });
}

/*
 ** update_category
 */
#[test]
// test if category updator is forum sudo
fn set_category_deleted() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );

        set_category_deleted_mock(origin, category_id, Ok(()));
    });
}

/*
 ** create_thread
 */
#[test]
// test if thread creator is valid forum user
fn create_thread() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);

    build_test_externalities(config).execute_with(|| {
        let forum_user_id = create_forum_user_mock(forum_sudo, Ok(()));
        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );
        create_thread_mock(
            origin.clone(),
            forum_user_id,
            category_id,
            good_thread_title(),
            good_thread_text(),
            Ok(()),
        );
    });
}

/*
 ** moderate_thread
 */

#[test]
// test if thread moderator registered as valid moderator
fn moderate_thread() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        let forum_user_id = create_forum_user_mock(forum_sudo, Ok(()));
        let moderator_id = create_moderator_mock(forum_sudo, Ok(()));
        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );
        set_moderator_category_mock(origin.clone(), moderator_id, category_id, true, Ok(()));
        let thread_id = create_thread_mock(
            origin.clone(),
            forum_user_id,
            category_id,
            good_thread_title(),
            good_thread_text(),
            Ok(()),
        );
        moderate_thread_mock(origin, moderator_id, thread_id, good_rationale(), Ok(()));
    });
}

/*
 ** add_post
 */

#[test]
// test if post origin registered as forum user
fn add_post() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        let forum_user_id = create_forum_user_mock(forum_sudo, Ok(()));

        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );

        let thread_id = create_thread_mock(
            origin.clone(),
            forum_user_id,
            category_id,
            good_thread_title(),
            good_thread_text(),
            Ok(()),
        );
        create_post_mock(
            origin.clone(),
            forum_user_id,
            thread_id,
            good_post_text(),
            Ok(()),
        );
    });
}

/*
 ** moderate_post
 */

#[test]
// test if post moderator registered
fn moderate_post() {
    let config = default_genesis_config();
    let forum_sudo = config.forum_sudo;
    let origin = OriginType::Signed(forum_sudo);
    build_test_externalities(config).execute_with(|| {
        let forum_user_id = create_forum_user_mock(forum_sudo, Ok(()));
        let moderator_id = create_moderator_mock(forum_sudo, Ok(()));

        let category_id = create_category_mock(
            origin.clone(),
            None,
            good_category_title(),
            good_category_description(),
            Ok(()),
        );
        set_moderator_category_mock(origin.clone(), moderator_id, category_id, true, Ok(()));

        let thread_id = create_thread_mock(
            origin.clone(),
            forum_user_id,
            category_id,
            good_thread_title(),
            good_thread_text(),
            Ok(()),
        );
        let post_id = create_post_mock(
            origin.clone(),
            forum_user_id,
            thread_id,
            good_post_text(),
            Ok(()),
        );
        moderate_post_mock(
            origin.clone(),
            moderator_id,
            post_id,
            good_rationale(),
            Ok(()),
        );
    });
}
