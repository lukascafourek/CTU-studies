create table address
(
    id serial primary key,
    post_code int4 not null,
    street varchar(100) not null,
    house_number int4 not null
);

create table genre
(
    id serial primary key,
    name varchar(100) not null
);

create table author
(
    id serial primary key,
    first_name varchar(100) not null,
    sur_name varchar(100) not null,
    address_id int4,
    foreign key(address_id) references address on delete set null
);

create table publisher
(
    id serial primary key,
    name varchar(100) not null,
    address_id int4,
    foreign key(address_id) references address on delete set null
);

create table library
(
    id serial primary key,
    name varchar(100) not null,
    address_id int4,
    foreign key(address_id) references address on delete set null
);

create table book
(
    id serial primary key,
    isbn varchar(50) not null unique,
    name varchar(256) not null,
    published date not null,
    publisher_id int4,
    genre_id int4,
    library_id int4,
    foreign key(genre_id) references genre on delete set null,
    foreign key(publisher_id) references publisher on delete set null,
    foreign key(library_id) references library on delete set null
);

create table contracts
(
    publisher_id int4 not null,
    author_id int4 not null,
    primary key (publisher_id, author_id),
    foreign key(publisher_id) references publisher on delete set null,
    foreign key(author_id) references author on delete set null
);

create table authorship
(
    book_id int4 not null,
    author_id int4 not null,
    primary key (book_id, author_id),
    foreign key(book_id) references book on delete cascade,
    foreign key(author_id) references author on delete cascade
);

