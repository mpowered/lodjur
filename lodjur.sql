CREATE TABLE revisions (
    id text PRIMARY KEY,
    "time" timestamp with time zone NOT NULL
);

CREATE TABLE builds (
    id uuid PRIMARY KEY,
    revision__id text REFERENCES revisions (id) ON DELETE cascade,
    start_time timestamp with time zone NOT NULL,
    finish_time timestamp with time zone NOT NULL,
    status text NOT NULL,
    started_by text NOT NULL
);

CREATE TABLE deploys (
    id uuid PRIMARY KEY,
    revision__id text REFERENCES revisions (id) ON DELETE cascade,
    start_time timestamp with time zone NOT NULL,
    finish_time timestamp with time zone NOT NULL,
    status text NOT NULL,
    started_by text NOT NULL,
    target text NOT NULL
);

CREATE TABLE checks (
    id uuid PRIMARY KEY,
    revision__id text REFERENCES revisions (id) ON DELETE cascade,
    start_time timestamp with time zone NOT NULL,
    finish_time timestamp with time zone NOT NULL,
    status text NOT NULL,
    started_by text NOT NULL,
    application text NOT NULL,
    examples integer NOT NULL,
    failed integer NOT NULL,
    pending integer NOT NULL,
    duration double precision NOT NULL
);

CREATE TABLE check_examples (
    id serial PRIMARY KEY,
    check__id uuid REFERENCES checks (id) ON DELETE cascade,
    description text NOT NULL,
    full_description text NOT NULL,
    status text NOT NULL,
    file_path text NOT NULL,
    line_number integer NOT NULL,
    "exception" jsonb NOT NULL
);

