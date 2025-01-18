package com.silectis.sql.audit

import com.silectis.sql.{ColumnReference, ColumnAlias, Query, QueryColumn, Subquery, TableAlias, TableReference}
import com.silectis.sql.parse.SqlParser
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.util.Try

class QueryAuditor extends LazyLogging {
  private val parser = new SqlParser

  // TODO add a couple more unit tests for getColumnNames and auditTailRec

  /**
   * Remove all literal columns and extract column names
   * @param columns the list of columns to convert
   */
  private def getColumnNames(columns: Seq[QueryColumn]): Seq[String] = {
    columns.collect {
      case ColumnAlias(ColumnReference(columnName), _)  => columnName
      case ColumnReference(columnName)                  => columnName
    }
  }

  /**
   * Tail recursive function to find the innermost query and return its "from" table and selected columns
   * @param query a query or subquery to convert
   */
  @tailrec
  private def auditTailRec(query: Query): QueryAuditResult = {
    query.from match {
      case Some(tr@TableReference(_, _))  => QueryAuditResult(Some(tr), getColumnNames(query.columns))
      case Some(TableAlias(tr, _))        => QueryAuditResult(Some(tr), getColumnNames(query.columns))
      case Some(Subquery(innerQuery, _))  => auditTailRec(innerQuery)
      case _                              => QueryAuditResult(None,  getColumnNames(query.columns))
    }
  }

  /**
   * Audit which table and columns are being accessed by the provided sql
   * @param sql a string of sql to audit. See SqlParser.scala for limitations.
   */
  def audit(sql: String): Try[QueryAuditResult] = {
    Try(parser.parse(sql))
      .map { query =>
        logger.debug(s"""Parsed query "$sql" as $query""")
        auditTailRec(query)
      }
  }
}
