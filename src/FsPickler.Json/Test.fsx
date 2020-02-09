#I "bin/Debug/net45"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open System


let d = DateTimeOffset(new DateTime(657404692696240000L)).ToLocalTime()
let tz = TimeZoneInfo.FindSystemTimeZoneById("GMT Standard Time")
TimeZoneInfo.ConvertTime(d, tz)

let d' = DateTime.Parse(d.ToString())

TimeZoneInfo.Local.GetUtcOffset(d')

let ticks, offset = d.Ticks, TimeZoneInfo.Local.GetUtcOffset(d)

//---------------------------------------

let dto = DateTimeOffset(d.Ticks, offset).ToUniversalTime().UtcDateTime

TimeZoneInfo.ConvertTime(dto, TimeZoneInfo.Local)

TimeZoneInfo.Local.IsDaylightSavingTime(d)

let tz = TimeZoneInfo.FindSystemTimeZoneById("GMT Standard Time")
tz.GetUtcOffset(dto)

let utcd = dto.UtcDateTime.ToLocalTime()

dto.LocalDateTime = d

dto.UtcDateTime.Ticks

dto.LocalDateTime

DateTimeOffset(d).LocalDateTime = d