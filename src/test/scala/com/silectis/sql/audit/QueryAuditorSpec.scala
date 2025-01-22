package com.silectis.sql.audit

import com.silectis.sql.{SqlException, TableReference}
import com.silectis.test.UnitSpec

import scala.util.Success

/**
  * @author noahvolson
  */
class QueryAuditorSpec extends UnitSpec {
  val auditor = new QueryAuditor

  it should "return failure for an invalid SQL query" in {
    a[SqlException] should be thrownBy
      auditor.audit("not valid sql").get
  }

  it should "return the selected table, schema, and columns" in {
    auditor.audit("select col1, col2 from schema.table1").get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col1", "col2"))
  }

  it should "not return literal columns" in {
    auditor.audit("select 1, 'two', col3 from schema.table1").get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col3"))
  }

  it should "return columns referenced by functions" in {
    auditor.audit("select sum(col1, avg(col2, col3)) from schema.table1").get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col1", "col2", "col3"))
  }

  it should "return aliased columns, not the aliases themselves" in {
    auditor.audit("select col1 as x, col2 as y from schema.table1").get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col1", "col2"))
  }

  it should "return aliased tables, not the aliases themselves" in {
    auditor.audit("select col1, col2 from schema.table1 as alias").get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col1", "col2"))
  }

  it should "not return a table if the query does not contain a table reference" in {
    auditor.audit("select 1 as one, 'two'").get shouldBe QueryAuditResult(None, Seq())
  }

  it should "return the columns, table, and schema of the innermost subquery" in {
    auditor.audit(
      """
        | select x, y, z
        | from (
        |   select
        |     a as x,
        |     b as y,
        |     c as z
        |   from (
        |     select
        |       col1 as a,
        |       col2 as b,
        |       col3 as c
        |     from schema.table1
        |   ) as a1
        | ) as a2
        |""".stripMargin).get shouldBe QueryAuditResult(Some(TableReference(Some("schema"), "table1")), Seq("col1", "col2", "col3"))
  }
}
