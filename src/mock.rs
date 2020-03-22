#![cfg(test)]

use crate::*;

use sp_core::H256;

use crate::{GenesisConfig, Module, Trait};

use frame_support::{impl_outer_dispatch, impl_outer_event, impl_outer_origin, parameter_types};
use sp_runtime::{
    testing::Header,
    traits::{BlakeTwo256, IdentityLookup},
    Perbill,
};

impl_outer_origin! {
    pub enum Origin for Runtime {}
}

impl_outer_dispatch! {
    pub enum Call for Runtime where origin: Origin {
        balances::Balances,
    }
}

pub mod forum_mod {
    pub use super::super::*;
    pub use crate::Event;
}

impl_outer_event! {
    pub enum TestEvent for Runtime {
        system<T>,
        balances<T>,
        forum_mod<T>,
    }
}

type BlockNumber = u64;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Runtime;

parameter_types! {
    pub const BlockHashCount: u64 = 250;
    pub const MaximumBlockWeight: u32 = 1024;
    pub const MaximumBlockLength: u32 = 2 * 1024;
    pub const AvailableBlockRatio: Perbill = Perbill::one();
    pub const MinimumPeriod: u64 = 5;
}

impl system::Trait for Runtime {
    type Call = ();
    type AccountId = u64;
    type Lookup = IdentityLookup<Self::AccountId>;
    type Index = u64;
    type BlockNumber = BlockNumber;
    type Hash = H256;
    type Hashing = BlakeTwo256;
    type Header = Header;
    type Event = TestEvent;
    type Origin = Origin;
    type BlockHashCount = BlockHashCount;
    type MaximumBlockWeight = MaximumBlockWeight;
    type MaximumBlockLength = MaximumBlockLength;
    type AvailableBlockRatio = AvailableBlockRatio;
    type Version = ();
    type ModuleToIndex = ();
    type AccountData = balances::AccountData<u64>;
    type OnNewAccount = ();
    type OnKilledAccount = ();
}

impl timestamp::Trait for Runtime {
    type Moment = u64;
    type OnTimestampSet = ();
    type MinimumPeriod = MinimumPeriod;
}

parameter_types! {
    pub const ExistentialDeposit: u64 = 500;
}

impl balances::Trait for Runtime {
    type AccountStore = System;
    type Balance = u64;
    type Event = TestEvent;
    type DustRemoval = ();
    type ExistentialDeposit = ExistentialDeposit;
    // type AccountStore = System;
}

parameter_types! {
    pub const DepositPerThread: u64 = 1_000;
    pub const DepositPerPost: u64 = 1_000;
    pub const DepositTitlePerByte: u64 = 1_000;
    pub const DepositTextPerByte: u64 = 1_000;
}

impl Trait for Runtime {
    type Event = TestEvent;
    type ForumUserId = u64;
    type ModeratorId = u64;
    type CategoryId = u64;
    type ThreadId = u64;
    type PostId = u64;
    type Currency = Balances;
    type DepositPerThread = DepositPerThread;
    type DepositPerPost = DepositPerPost;
    type DepositTitlePerByte = DepositTitlePerByte;
    type DepositTextPerByte = DepositTextPerByte;
}

#[derive(Clone)]
pub enum OriginType {
    Signed(<Runtime as system::Trait>::AccountId),
    //Inherent, <== did not find how to make such an origin yet
    Root,
}

pub fn mock_origin(origin: OriginType) -> mock::Origin {
    match origin {
        OriginType::Signed(account_id) => Origin::signed(account_id),
        //OriginType::Inherent => Origin::inherent,
        OriginType::Root => system::RawOrigin::Root.into(), //Origin::root
    }
}

pub const NOT_FORUM_SUDO_ORIGIN_ID: <Runtime as system::Trait>::AccountId = 111;

pub const NOT_FORUM_SUDO_ORIGIN: OriginType = OriginType::Signed(NOT_FORUM_SUDO_ORIGIN_ID);

pub const INVLAID_CATEGORY_ID: <Runtime as Trait>::CategoryId = 333;

pub const NOT_REGISTER_MODERATOR_ID: <Runtime as Trait>::ModeratorId = 666;

pub fn good_category_title() -> Vec<u8> {
    b"Great new category".to_vec()
}

pub fn good_category_description() -> Vec<u8> {
    b"This is a great new category for the forum".to_vec()
}

pub fn good_thread_title() -> Vec<u8> {
    b"Great new thread".to_vec()
}

pub fn good_thread_text() -> Vec<u8> {
    b"The first post in this thread".to_vec()
}

pub fn good_post_text() -> Vec<u8> {
    b"A response in the thread".to_vec()
}

pub fn good_rationale() -> Vec<u8> {
    b"This post violates our community rules".to_vec()
}

pub fn create_forum_user_mock(
    account_id: <Runtime as system::Trait>::AccountId,
    result: DispatchResult,
) -> <Runtime as Trait>::ForumUserId {
    let forum_user_id = TestForumModule::next_forum_user_id();
    assert_eq!(TestForumModule::create_forum_user(account_id,), result);
    if result.is_ok() {
        let forum_user = ForumUser {
            role_account: account_id,
        };
        assert_eq!(TestForumModule::forum_user_by_id(forum_user_id), forum_user,);
        assert_eq!(TestForumModule::next_forum_user_id(), forum_user_id + 1);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::ForumUserCreated(forum_user_id))
        );
    };

    forum_user_id
}

pub fn create_moderator_mock(
    account_id: <Runtime as system::Trait>::AccountId,
    result: DispatchResult,
) -> <Runtime as Trait>::ModeratorId {
    let moderator_id = TestForumModule::next_moderator_id();
    assert_eq!(TestForumModule::create_moderator(account_id,), result);
    if result.is_ok() {
        let moderator = Moderator {
            role_account: account_id,
        };
        assert_eq!(TestForumModule::moderator_by_id(moderator_id), moderator);
        assert_eq!(TestForumModule::next_moderator_id(), moderator_id + 1);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::ModeratorCreated(moderator_id))
        );
    };
    moderator_id
}

pub fn create_category_mock(
    origin: OriginType,
    parent: Option<<Runtime as Trait>::CategoryId>,
    title: Vec<u8>,
    description: Vec<u8>,
    result: DispatchResult,
) -> <Runtime as Trait>::CategoryId {
    let category_id = TestForumModule::next_category_id();
    assert_eq!(
        TestForumModule::create_category(
            mock_origin(origin),
            parent,
            title.clone(),
            description.clone(),
        ),
        result
    );
    if result.is_ok() {
        assert_eq!(TestForumModule::next_category_id(), category_id + 1);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::CategoryCreated(category_id))
        );
    }
    category_id
}

pub fn create_thread_mock(
    origin: OriginType,
    forum_user_id: <Runtime as Trait>::ForumUserId,
    category_id: <Runtime as Trait>::CategoryId,
    title: Vec<u8>,
    text: Vec<u8>,
    result: DispatchResult,
) -> <Runtime as Trait>::ThreadId {
    let thread_id = TestForumModule::next_thread_id();
    println!("Thread id is {:?}", thread_id);
    assert_eq!(
        TestForumModule::create_thread(
            mock_origin(origin.clone()),
            forum_user_id,
            category_id,
            title.clone(),
            text.clone(),
        ),
        result
    );
    if result.is_ok() {
        assert_eq!(TestForumModule::next_thread_id(), thread_id + 1);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::ThreadCreated(thread_id))
        );
    }
    thread_id
}

pub fn create_post_mock(
    origin: OriginType,
    forum_user_id: <Runtime as Trait>::ForumUserId,
    thread_id: <Runtime as Trait>::ThreadId,
    text: Vec<u8>,
    result: DispatchResult,
) -> <Runtime as Trait>::PostId {
    let post_id = TestForumModule::next_post_id();
    assert_eq!(
        TestForumModule::add_post(
            mock_origin(origin.clone()),
            forum_user_id,
            thread_id,
            text.clone(),
        ),
        result
    );
    if result.is_ok() {
        assert_eq!(TestForumModule::next_post_id(), post_id + 1);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::PostAdded(post_id))
        );
    };
    post_id
}

pub fn set_forum_sudo_mock(
    origin: OriginType,
    new_forum_sudo: Option<<Runtime as system::Trait>::AccountId>,
    result: DispatchResult,
) {
    let old_forum_sudo = TestForumModule::forum_sudo();

    assert_eq!(
        TestForumModule::set_forum_sudo(mock_origin(origin), new_forum_sudo),
        result
    );

    if result.is_ok() {
        assert_eq!(TestForumModule::forum_sudo(), new_forum_sudo);
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::ForumSudoSet(old_forum_sudo, new_forum_sudo,))
        );
    };
}

pub fn set_moderator_category_mock(
    origin: OriginType,
    moderator_id: <Runtime as Trait>::ModeratorId,
    category_id: <Runtime as Trait>::CategoryId,
    new_value: bool,
    result: DispatchResult,
) -> <Runtime as Trait>::CategoryId {
    assert_eq!(
        TestForumModule::set_moderator_category(
            mock_origin(origin),
            moderator_id,
            category_id,
            new_value
        ),
        result
    );
    if result.is_ok() {
        assert_eq!(
            TestForumModule::category_by_moderator(category_id, moderator_id),
            new_value
        );
    };
    category_id
}

pub fn set_category_archived_mock(
    origin: OriginType,
    category_id: <Runtime as Trait>::CategoryId,
    result: DispatchResult,
) -> <Runtime as Trait>::CategoryId {
    assert_eq!(
        TestForumModule::set_category_archived(mock_origin(origin), category_id,),
        result
    );
    if result.is_ok() {
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::CategoryArchieved(category_id,))
        );
    };
    category_id
}

pub fn set_category_deleted_mock(
    origin: OriginType,
    category_id: <Runtime as Trait>::CategoryId,
    result: DispatchResult,
) -> <Runtime as Trait>::CategoryId {
    assert_eq!(
        TestForumModule::set_category_deleted(mock_origin(origin), category_id,),
        result
    );
    if result.is_ok() {
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::CategoryDeleted(category_id,))
        );
    };
    category_id
}

pub fn moderate_thread_mock(
    origin: OriginType,
    moderator_id: <Runtime as Trait>::ModeratorId,
    thread_id: <Runtime as Trait>::ThreadId,
    rationale: Vec<u8>,
    result: DispatchResult,
) -> <Runtime as Trait>::ThreadId {
    assert_eq!(
        TestForumModule::moderate_thread(mock_origin(origin), moderator_id, thread_id, rationale),
        result
    );
    if result.is_ok() {
        assert_eq!(
            TestForumModule::thread_by_id(thread_id)
                .moderation
                .is_some(),
            true
        );
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::ThreadModerated(thread_id,))
        );
    }
    thread_id
}

pub fn moderate_post_mock(
    origin: OriginType,
    moderator_id: <Runtime as Trait>::ModeratorId,
    post_id: <Runtime as Trait>::PostId,
    rationale: Vec<u8>,
    result: DispatchResult,
) -> <Runtime as Trait>::PostId {
    assert_eq!(
        TestForumModule::moderate_post(mock_origin(origin), moderator_id, post_id, rationale),
        result
    );
    if result.is_ok() {
        assert_eq!(
            TestForumModule::post_by_id(post_id).moderation.is_some(),
            true
        );
        assert_eq!(
            System::events().last().unwrap().event,
            TestEvent::forum_mod(RawEvent::PostModerated(post_id,))
        );
    }

    post_id
}

pub fn default_genesis_config() -> GenesisConfig<Runtime> {
    create_genesis_config()
}

pub fn create_genesis_config() -> GenesisConfig<Runtime> {
    GenesisConfig::<Runtime> {
        forum_user_by_id: vec![],
        next_forum_user_id: 1,
        moderator_by_id: vec![],
        next_moderator_id: 1,
        category_by_id: vec![],
        thread_by_id: vec![],
        next_thread_id: 1,
        post_by_id: vec![],
        next_post_id: 1,
        direct_threads_by_id: vec![],
        next_category_id: 1,
        posts_by_thread_id: vec![],
        sub_categories_by_id: vec![],
        forum_sudo: 33,
        category_by_moderator: vec![],
        category_title_constraint: InputLengthConstraint { min: 10, max: 140 },
        category_description_constraint: InputLengthConstraint { min: 10, max: 140 },
        thread_title_constraint: InputLengthConstraint { min: 3, max: 43 },
        post_text_constraint: InputLengthConstraint { min: 1, max: 1001 },
        moderation_rationale_constraint: InputLengthConstraint { min: 10, max: 2000 },
    }
}

pub fn build_test_externalities(config: GenesisConfig<Runtime>) -> runtime_io::TestExternalities {
    let mut t = system::GenesisConfig::default()
        .build_storage::<Runtime>()
        .unwrap();

    config.assimilate_storage(&mut t).unwrap();

    t.into()
}

pub type System = system::Module<Runtime>;
pub type Balances = balances::Module<Runtime>;
pub type TestForumModule = Module<Runtime>;
