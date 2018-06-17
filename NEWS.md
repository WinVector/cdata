
# cdata 0.7.2 2018/06/17

 * re-export more of wrapr

# cdata 0.7.1 2018/06/16

 * Documentation fixes.
 * Don't export cols().
 * Reduce wrapr re-export.
 * More rows in qlook().

# cdata 0.7.0 2018/04/09

 * Narrow dependencies.
 * Switch to dbExecute() (sparklyr seems to have that now).
 * Non-DB implementations for local data case.
 * Remove deprecated fns.

# cdata 0.6.0 2018/03/12

 * Add cols() method.
 * Add doi link in DESCRIPTION (CRAN request).
 * Use build_frame(), draw_frame(), and qchar_frame (quoted frame) from wrapr 1.3.0.

# cdata 0.5.2 2018/01/20

 * Remove append based row binding (seems to have some issues on Spark).
 * Deprecate old methods.

# cdata 0.5.1 2018/01/03

 * New naming convention.
 * Doc fixes.
 * Better table lifetime controls.
 * Move to wrapr 1.0.2.
 * Move grepdf out of package.
 * Add row binder.
 * Add map_fields.
 * Add winvector_temp_db_handle support.

# cdata 0.5.0 2017/11/13

 * query-based re-implementation
 * fluid data workflow.
 * remove dplyr and tidyr dependence
 
# cdata 0.1.7 2017/10/31

 * Better error msgs.

# cdata 0.1.6 2017/10/12

 * work around empty keyset issues.
 * add column control.

# cdata 0.1.5 2017/07/04

 * Allow NA in key columns.
 * Add optional class annotation when moving values to rows.

# cdata 0.1.1 2017/05/05

 * ungroup before calculating distinct.

# cdata 0.1.0 2017/03/28

 * First release.
