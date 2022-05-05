CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"username" VARCHAR NOT NULL,"hashed_password" BLOB NOT NULL,"created_timestamp" TIMESTAMP NOT NULL,"last_login" TIMESTAMP NULL,CONSTRAINT "unique_user_identifier" UNIQUE ("identifier"),CONSTRAINT "unique_username" UNIQUE ("username"));
CREATE TABLE "customer"("id" INTEGER PRIMARY KEY,"user" BLOB NOT NULL,"stripe_customer" VARCHAR NOT NULL,CONSTRAINT "unique_stripe_customer" UNIQUE ("user","stripe_customer"));
CREATE TABLE "subscription"("id" INTEGER PRIMARY KEY,"user" BLOB NOT NULL,"end" TIMESTAMP NOT NULL,CONSTRAINT "unique_subscription_user" UNIQUE ("user"));
CREATE TABLE "intray_item"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"user_id" BLOB NOT NULL,"type" VARCHAR NOT NULL,"contents" BLOB NOT NULL,"created" TIMESTAMP NOT NULL,CONSTRAINT "unique_item_identifier" UNIQUE ("identifier","user_id"));
CREATE TABLE "access_key"("id" INTEGER PRIMARY KEY,"identifier" BLOB NOT NULL,"user" BLOB NOT NULL,"name" VARCHAR NOT NULL,"hashed_key" BLOB NOT NULL,"created_timestamp" TIMESTAMP NOT NULL,"permissions" VARCHAR NOT NULL,CONSTRAINT "unique_access_key_identifier" UNIQUE ("identifier","user"));

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Intray.Data.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.
