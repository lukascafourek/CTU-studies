create table City
(
    id serial primary key,
    name varchar(100) not null
);

create table Address
(
    id serial primary key,
    street varchar(100) not null,
    house_number int4 not null,
    city_id int4,
    foreign key(city_id) references City on delete set null
);

create table Genre
(
    id serial primary key,
    name varchar(100) not null
);

create table Author
(
    id serial primary key,
    first_name varchar(100) not null,
    sur_name varchar(100) not null,
    email varchar(100) not null,
    address_id int4,
    foreign key(address_id) references Address on delete set null
);

create table Publisher
(
    id serial primary key,
    name varchar(100) not null,
    address_id int4,
    foreign key(address_id) references Address on delete set null
);

create table Library
(
    id serial primary key,
    name varchar(100) not null,
    address_id int4,
    foreign key(address_id) references Address on delete set null
);

create table Book
(
    id serial primary key,
    isbn varchar(50) not null unique,
    name varchar(256) not null,
    published date not null,
    publisher_id int4,
    genre_id int4,
    library_id int4,
    foreign key(genre_id) references Genre on delete set null,
    foreign key(publisher_id) references Publisher on delete set null,
    foreign key(library_id) references Library on delete set null
);

create table Contracts
(
    publisher_id int4 not null,
    author_id int4 not null,
    primary key (publisher_id, author_id),
    foreign key(publisher_id) references Publisher on delete set null,
    foreign key(author_id) references Author on delete set null
);

create table Authorship
(
    book_id int4 not null,
    author_id int4 not null,
    primary key (book_id, author_id),
    foreign key(book_id) references Book on delete set null,
    foreign key(author_id) references Author on delete set null
);
