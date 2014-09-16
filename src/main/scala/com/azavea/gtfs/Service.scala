package com.azavea.gtfs

import com.github.nscala_time.time.Imports._

/**
 * Service availability calendar, combining both regular service and exceptions
 * Source: calendar.txt, calendar_dates.txt
 */
case class Service(week: ServiceCalendar, exceptions: Seq[ServiceException]) {
  def id = week.service_id

  def activeOn(dt: LocalDate) = {
    exceptions.find(_.date == dt) match {
      case Some(e) => e.addService    //check exception first
      case None => week.activeOn(dt)  //default
    }
  }
}

object Service {
   def collate(weeks: Seq[ServiceCalendar], dates: Seq[ServiceException]): Seq[Service] = {
    val ids = (weeks.map(_.service_id) union dates.map(_.service_id)).toSet
    val datesMap = dates.groupBy(_.service_id)
    val weeksMap = weeks.groupBy(_.service_id)

    ids map { sid =>
      (weeksMap.get(sid), datesMap.get(sid)) match {
        case (Some(Seq(week)), Some(dates)) => Service(week, dates)
        case (Some(Seq(week)), None) => Service(week, Nil)
        case (None, Some(dates)) =>
          //the dates are meaningless in this case
          val week = ServiceCalendar(sid,new LocalDate(0,1,1), new LocalDate(0,1,1), Array.fill(7)(false))
          Service(week, dates)
        case _ => sys.error(s"Unhandled case when building service instance: $sid")
      }
    }
  }.toSeq
}