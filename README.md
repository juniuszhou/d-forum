# Substrate Forum Module
## Overview
The module is a pallet based on Substrate to create a decentralized forum. It could be deployed as a module in Substrate runtime.

## Structure of Forum
1. Category: include the description and some child categories. Thread can be created in the category.
   Category has three status: valie, archived and deleted.
2. Thread: include the title to introduce a topic for people to discuss
3. Post: any forum user can submit a post in thread to express his/her opinion about the thread
   
## Roles
There are three roles in the forum:
1. Sudo: can create category, add forum user and moderator. set category as archived and deleted
2. Moderator: find out the misbehavior, can moderate the thread and post
3. Forum user: can create thread and submit post throught reserving some currency

## Moderation
Moderator can moderate the thread or post if any invalid content. If the thread and post marked as moderated, then the currency reserved can be paid back. 

## Currency reservation
Some currency should be reserved when any forum user submit thread or post. The currency will be unreserved after the category set as archievd.

## Data migration
The forum pallet may get lot of requirement and the need update the data structure.
The process is implemented to migrate data from old version during runtime upgrade.
