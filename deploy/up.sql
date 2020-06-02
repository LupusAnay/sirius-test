create database sirius;

use sirius;

create table nodes
(
	id serial not null constraint nodes_pk primary key,
	label varchar(255) not null
);

create table links
(
	id serial not null constraint links_pk primary key,
	from_id integer constraint links_nodes_id_fk references nodes,
	to_id integer constraint links_nodes_id_fk_2 references nodes,
	constraint links_unique_together unique (from_id, to_id),
	constraint links_loops check (from_id != to_id)
);
