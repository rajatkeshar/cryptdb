
(DEFUN ADJUST-MAIN ()
  "onion adjuster for database: burrows+one, created: 12/8/2013 3:30:10"
  (LET* ((DB "dbnew")
         (CONN
          (CLSQL-SYS:CONNECT (LIST "127.0.0.1" DB "root" "letmein" 3307)
                             :DATABASE-TYPE :MYSQL :IF-EXISTS :NEW
                             :MAKE-DEFAULT NIL)))
    (UNWIND-PROTECT
        (PROGN
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_commentmeta" "comment_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_commentmeta" "meta_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_commentmeta" "meta_key" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_commentmeta" "meta_value" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_ID" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_approved" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_author" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_author_email" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_author_url" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_content" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_date" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_date_gmt" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_parent" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_post_ID" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "comment_post_ID" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_comments" "user_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_links" "link_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_links" "link_updated" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_options" "autoload" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_options" "option_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_options" "option_name" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_options" "option_name" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_key" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_key" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_key" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_value" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "meta_value" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "post_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_postmeta" "post_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "ID" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "menu_order" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_author" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_date" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_date_gmt" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_mime_type" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_modified" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_modified_gmt" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_name" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_name" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_parent" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_parent" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_status" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_title" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "post_type" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_posts" "to_ping" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_relationships" "object_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_relationships" "object_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_relationships" "term_taxonomy_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_relationships" "term_taxonomy_id" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB
                 '("wp_term_relationships" "term_taxonomy_id" "oPLAIN"
                   "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_taxonomy" "count" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_taxonomy" "taxonomy" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_taxonomy" "term_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_term_taxonomy" "term_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB
                 '("wp_term_taxonomy" "term_taxonomy_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_terms" "name" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_terms" "name" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_terms" "slug" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_terms" "term_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_usermeta" "meta_key" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_usermeta" "meta_value" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_usermeta" "umeta_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_usermeta" "user_id" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_usermeta" "user_id" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "ID" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "display_name" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_email" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_login" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_login" "oOrder" "OPE"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_nicename" "oEq" "DET"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_registered" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN)
         (CLSQL-SYS:QUERY
          (APPLY #'FORMAT NIL "SET @cryptdb='adjust',
                                         @database='~A',
                                         @table='~A',
                                         @field='~A',
                                         @~A='~A'"
                 DB '("wp_users" "user_url" "oPLAIN" "PLAINVAL"))
          :DATABASE CONN))
      (CLSQL-SYS:DISCONNECT :DATABASE CONN)))
  NIL)
