-- schema for the database

/*DROP SCHEMA IF EXISTS voting CASCADE;
CREATE SCHEMA voting;
SET SEARCH_PATH to voting, public; */

DROP TABLE IF EXISTS AllocatedVote;
DROP TABLE IF EXISTS Vote;
DROP TABLE IF EXISTS Choice;
DROP TABLE IF EXISTS Topic;
DROP TABLE IF EXISTS Member;


CREATE TABLE Member (
  id SERIAL PRIMARY KEY,
  username VARCHAR(128) NOT NULL,
  dateJoined timestamptz NOT NULL,
  email VARCHAR(255)
);


CREATE TABLE Topic (
  id SERIAL PRIMARY KEY, 
  name VARCHAR(128) NOT NULL,
  description TEXT,
  proposedBy INT NOT NULL REFERENCES Member,
  startTime timestamptz NOT NULL,
  endTime timestamptz NOT NULL
  -- TODO : Add specific ending conditions?
);


CREATE TABLE Choice (
  id SERIAL PRIMARY KEY,
  name VARCHAR(128) NOT NULL,
  topicID INT NOT NULL REFERENCES Topic,
  description TEXT,
  proposedBy INT NOT NULL REFERENCES Member,
  dateProposed timestamptz NOT NULL
);

-- A vote (allocation) which has already happened
CREATE TABLE Vote (
  voterID INT NOT NULL REFERENCES Member,  
  choiceID INT NOT NULL REFERENCES Choice,
  amount INT NOT NULL,  
  mostRecent timestamptz NOT NULL,
  comment TEXT
);


CREATE TABLE AllocatedVote (
  memberID INT NOT NULL REFERENCES Member,
  topicID INT NOT NULL REFERENCES Topic,
  votesAllocated INT NOT NULL,
  PRIMARY KEY (memberID, topicID)
);

-- Data copied from CSV files.
\COPY Member FROM 'mock/member.csv' DELIMITER ',' CSV header;
\COPY Topic FROM 'mock/topic.csv' DELIMITER ',' CSV header;
\COPY Choice FROM 'mock/choice.csv' DELIMITER ',' CSV header;
\COPY Vote FROM 'mock/vote.csv' DELIMITER ',' CSV header;
\COPY AllocatedVote FROM 'mock/allocatedvote.csv' DELIMITER ',' CSV header;

