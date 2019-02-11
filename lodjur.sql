CREATE TABLE revisions (
    id text PRIMARY KEY,
    "time" timestamp with time zone NOT NULL
);

CREATE TABLE builds (
    job__id uuid PRIMARY KEY,
    job__description text NOT NULL,
    job__revision__id text REFERENCES revisions (id),
    job__start_time timestamp with time zone NOT NULL,
    job__finish_time timestamp with time zone NOT NULL,
    job__status text NOT NULL,
    job__started_by text NOT NULL
);

CREATE TABLE deploys (
    job__id uuid PRIMARY KEY,
    job__description text NOT NULL,
    job__revision__id text REFERENCES revisions (id),
    job__start_time timestamp with time zone NOT NULL,
    job__finish_time timestamp with time zone NOT NULL,
    job__status text NOT NULL,
    job__started_by text NOT NULL,
    target text NOT NULL
);

CREATE TABLE checks (
    job__id uuid PRIMARY KEY,
    job__description text NOT NULL,
    job__revision__id text REFERENCES revisions (id),
    job__start_time timestamp with time zone NOT NULL,
    job__finish_time timestamp with time zone NOT NULL,
    job__status text NOT NULL,
    job__started_by text NOT NULL,
    examples integer NOT NULL,
    failed integer NOT NULL,
    pending integer NOT NULL,
    duration double precision NOT NULL
);

CREATE TABLE check_examples (
    id serial PRIMARY KEY,
    check__job__id uuid REFERENCES checks (job__id),
    description text NOT NULL,
    full_description text NOT NULL,
    status text NOT NULL,
    file_path text NOT NULL,
    line_number integer NOT NULL,
    "exception" jsonb NOT NULL
);

