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

CREATE TABLE rspecs (
    id serial4 PRIMARY KEY,
    job__id int4 references jobs (id) ON DELETE cascade,
    duration double precision NOT NULL,
    example_count integer NOT NULL,
    failure_count integer NOT NULL,
    pending_count integer NOT NULL
);

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

CREATE TABLE logs (
    id serial8 PRIMARY KEY,
    job__id int4 references jobs (id) ON DELETE cascade,
    created_at timestamp with time zone NOT NULL,
    "text" text NOT NULL
);

CREATE TABLE users (
    id int4 NOT NULL,
    login text NOT NULL,
    name text NULL,
    email text NULL,
    company text NULL,
    location text NULL,
    avatar_url text NULL,
    created_at timestamp with time zone NOT NULL
);
