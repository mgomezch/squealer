insert into "Product"
  ("code", "name"      , "price") values
  ('P1'  , 'Producto 1', '$100' ),
  ('P2'  , 'Producto 2', '$200' ),
  ('P3'  , 'Producto 3', '$300' );

\echo test 1

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";


insert into "Client"
  ("code"   , "name"                     , "date of birth") values
  ('0537821', 'Miguel Ambrosio'          , '1987-12-19'   ),
  ('0741295', 'Narbelyrilisleysy Oropeza', '1990-01-23'   );

\echo test 2

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

insert into "Order"
  ("code", "client -> code", "timestamp", "status") values
  ('O1'  , '0537821'       , now()      , 'placed'),
  ('O2'  , '0741295'       , now()      , 'placed'),
  ('O3'  , '0537821'       , now()      , 'placed');

\echo test 3

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

insert into "BillItem"
  ("order -> code", "product -> code", "quantity") values
  ('O1'           , 'P1'             ,         10),

  ('O2'           , 'P1'             ,          5),
  ('O2'           , 'P2'             ,          1),

  ('O3'           , 'P1'             ,          1),
  ('O3'           , 'P2'             ,         15),
  ('O3'           , 'P3'             ,          3)
;

\echo test 4

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "BillItem" set "quantity" = 42 where "order -> code" = 'O1' and "product -> code" = 'P1';

\echo test 5

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "Order" set "status" = 'approved' where "code" = 'O2';
\echo test 6a
update "Order" set "status" = 'approved' where "code" = 'O3';
\echo test 6b
update "Order" set "status" = 'shipped'  where "code" = 'O3';
\echo test 6c

\echo test 6

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "Order" set "client -> code" = null where "code" = 'O3';

\echo test 7

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "Client" set "name" = 'Narbelys Oropeza' where "code" = '0741295';

\echo test 8

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "Client" set "name" = 'Miguel Miguel' where "code" = '0537821';

\echo test 9

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "Product" set "price" = "price" * 1.12;

\echo test 10

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";

update "BillItem" set "product version" = null where "product -> code" = 'P3';

\echo test 11

table "Client"."version"; table "Order"."version"; table "Product"."version"; table "BillItem"."version";
