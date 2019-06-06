CREATE TABLE commits (
    id serial4 PRIMARY KEY,
    sha text NOT NULL,
    owner text NOT NULL,
    repo text NOT NULL,
    branch text NULL,
    message text NULL,
    author text NULL,
    author_email text NULL,
    committer text NULL,
    committer_email text NULL,
    "timestamp" timestamp with time zone NULL,
    UNIQUE (owner, repo, sha)
);

CREATE TABLE jobs (
    id serial4 PRIMARY KEY,
    name text NOT NULL,
    commit__id int4 references commits (id) ON DELETE cascade,
    action jsonb NOT NULL,
    status text NOT NULL,
    conclusion text NULL,
    created_at timestamp with time zone NOT NULL,
    started_at timestamp with time zone NULL,
    completed_at timestamp with time zone NULL,
    parent__id int4 REFERENCES jobs (id) ON DELETE cascade
);

CREATE INDEX jobs_parent_id ON jobs (parent__id);

CREATE TABLE rspecs (
    id serial4 PRIMARY KEY,
    job__id int4 references jobs (id) ON DELETE cascade,
    duration double precision NOT NULL,
    example_count integer NOT NULL,
    failure_count integer NOT NULL,
    pending_count integer NOT NULL
);

CREATE INDEX rspecs_job_id ON rspecs (job__id);

CREATE TABLE rspec_tests (
    id serial8 PRIMARY KEY,
    r_spec__id int4 REFERENCES rspecs (id) ON DELETE cascade,
    description text NOT NULL,
    full_description text NOT NULL,
    status text NOT NULL,
    file_path text NOT NULL,
    line_number integer NOT NULL,
    exception_class text NULL,
    exception_message text NULL,
    exception_backtrace text NULL
);

CREATE INDEX rspec_tests_rspec_id ON rspec_tests (r_spec__id);

CREATE INDEX rspec_tests_status ON rspec_tests (status);

CREATE INDEX rspec_tests_file_line ON rspec_tests (file_path, line_number);

CREATE TABLE logs (
    id serial8 PRIMARY KEY,
    job__id int4 references jobs (id) ON DELETE cascade,
    created_at timestamp with time zone NOT NULL,
    "text" text NOT NULL
);

CREATE INDEX logs_job_id_created_at ON logs (job__id, created_at);

CREATE TABLE users (
    id int8 PRIMARY KEY,
    login text NOT NULL,
    name text NULL,
    email text NULL,
    company text NULL,
    location text NULL,
    avatar_url text NULL,
    access_token text NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    last_login timestamp with time zone NOT NULL
);

CREATE TABLE organizations (
    id int8 PRIMARY KEY,
    login text NOT NULL,
    avatar_url text NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

CREATE TABLE installations (
    id int8 PRIMARY KEY
);

CREATE TABLE repositories (
    id int8 PRIMARY KEY,
    login text NOT NULL,
    owner text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    UNIQUE (owner, login)
);

CREATE TABLE user_accessible (
    user__id int4 references users (id) ON DELETE cascade,
    installation__id int4 references installations (id) ON DELETE cascade,
    repository__id int4 references repositories (id) ON DELETE cascade,
    seen_at timestamp with time zone NOT NULL,
    UNIQUE (user__id, installation__id, repository__id)
);