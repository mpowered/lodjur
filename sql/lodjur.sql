CREATE TABLE jobs (
    id serial PRIMARY KEY,
    name text NOT NULL,
    src_sha text NOT NULL,
    src_branch text NULL,
    src_owner text NOT NULL,
    src_repo text NOT NULL,
    src_message text NULL,
    src_committer text NULL,
    action jsonb NOT NULL,
    status text NOT NULL,
    conclusion text NULL,
    created_at timestamp with time zone NOT NULL,
    started_at timestamp with time zone NULL,
    completed_at timestamp with time zone NULL,
    parent__id int4 REFERENCES jobs (id) ON DELETE cascade
);

CREATE TABLE rspecs (
    id serial PRIMARY KEY,
    duration double precision NOT NULL,
    example_count integer NOT NULL,
    failure_count integer NOT NULL,
    pending_count integer NOT NULL
);

CREATE TABLE rspec_tests (
    id serial8 PRIMARY KEY,
    rspec_summary__id int4 REFERENCES rspecs (id) ON DELETE cascade,
    description text NOT NULL,
    full_description text NOT NULL,
    status text NOT NULL,
    file_path text NOT NULL,
    line_number integer NOT NULL,
    "exception" jsonb NOT NULL
);

