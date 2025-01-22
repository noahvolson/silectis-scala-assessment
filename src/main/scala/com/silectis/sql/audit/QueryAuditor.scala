package com.silectis.sql.audit

import com.silectis.sql.{ColumnAlias, ColumnExpression, ColumnReference, Query, QueryColumn, SqlFunction, Subquery, TableAlias, TableReference}
import com.silectis.sql.parse.SqlParser
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.util.Try

class QueryAuditor extends LazyLogging {
  private val parser = new SqlParser


  /**
   * Extract the string column names out of a column expression. Recursively extract function parameter columns
   * @param expr a column expression to extract names from
   */
  private def extractColumnNames(expr: ColumnExpression): Seq[String] = {
    expr match {
      case ColumnReference(columnName)  => Seq(columnName)
      case SqlFunction(_, parameters)   => parameters.flatMap(extractColumnNames) // Could have nested functions like sum(col1, avg(col2, col3))
      case _                            => Seq.empty // Don't return literals
    }
  }

  /**
   * Convert a sequence of QueryColumns to a sequence of string column names
   * @param columns the sequence of columns to convert
   */
  private def queryColumnsToStrings(columns: Seq[QueryColumn]): Seq[String] = {
    columns.foldLeft(Seq.empty[String]) { (acc, column) =>
      column match {
        case ColumnAlias(expr, _)   => acc ++ extractColumnNames(expr)
        case expr: ColumnExpression => acc ++ extractColumnNames(expr)
      }
    }
  }

  /**
   * Tail recursive function to find the innermost query and return its "from" table and selected columns
   * @param query a query or subquery to convert
   */
  @tailrec
  private def auditTailRec(query: Query): QueryAuditResult = {
    query.from match {
      case Some(tr: TableReference)       => QueryAuditResult(Some(tr), queryColumnsToStrings(query.columns))
      case Some(TableAlias(tr, _))        => QueryAuditResult(Some(tr), queryColumnsToStrings(query.columns))
      case Some(Subquery(innerQuery, _))  => auditTailRec(innerQuery)
      case _                              => QueryAuditResult(None,     queryColumnsToStrings(query.columns))
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
