CREATE TABLE links (
	source	varchar(1024) NOT NULL,
	target	varchar(1024) NOT NULL,
    PRIMARY KEY (source, target)
);

CREATE TABLE lambda (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(100) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE lambda_stems (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(100) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE lambda_soundex (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(4) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE lambda_m (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(100) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE lambda_mstems (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(100) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE lambda_msoundex (                                              
    world       varchar(1024) NOT NULL,
    formula     varchar(4) NOT NULL,
    frmcount    integer NOT NULL,
    tfidf       float,
    PRIMARY KEY (world, formula)
);

CREATE TABLE world_stem_lang (                                              
    world       varchar(1024) NOT NULL,
    language    varchar(100) NOT NULL,
    PRIMARY KEY (world)
);

CREATE TABLE pagerank (
    world       varchar(1024) NOT NULL,
    score       float DEFAULT 1.0 NOT NULL,
    PRIMARY KEY (world)
);
