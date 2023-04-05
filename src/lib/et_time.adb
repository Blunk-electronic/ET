-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DATE AND TIME                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:



package body et_time is

	function date_now return type_date is
		now		: constant time := clock;
		date	: string (1..19) := image (now, time_zone => utc_time_offset (now));
	begin
		date (11) := 'T'; -- inserts a T so that the result is "2017-08-17T14:17:25"
		return type_date (date);
	end date_now;

	
	function get_date (
		preamble : in boolean := true) 
		return string 
	is begin
		if preamble then
			return "date " & string (date_now);
		else
			return string (date_now);
		end if;
	end get_date;


	function to_string_full (
		time	: ada.calendar.time)
		return string
	is
		now : string (1..19) := image (time);
	begin
		now (11) := 'T'; -- inserts a T so that the result is "2017-08-17T14:17:25"
		return now;
	end to_string_full;
	

	function to_string_YMD (
		time	: ada.calendar.time)
		return string
	is
		now : string (1..10) := image (time) (1..10);
	begin
		return now;
	end to_string_YMD;

	
	function date_first return time is
		r : time := gnat.calendar.no_time; -- 1901-01-01
		--time_of (year => 1970, month => 01, day => 01, seconds => 1.0); -- return 1970-01-01
	begin
		return r;
	end date_first;
	

	function to_string (date : in type_date) return string is
	begin
		return string (date);
	end to_string;


	function to_date (
		date : in string) 
		return time 
	is begin
		-- The function "value" requires something like "2017-08-17 14:17:25".
		-- Since date provides only year, month and day, we append hours, minutes and seconds.
		return value (date & " 00:00:00");

		exception when event:
			--others => log (ERROR, text => "date invalid !", console => true);
			others => put_line ("ERROR ! Date invalid !");
				raise;
	end to_date;

	
	function date_valid (date : in type_date) return boolean is
	begin
		-- CS
		-- CS: call a procedure that says something like "date format invalid" or "date in stone age or date in future"
		return true;
	end date_valid;

	
end et_time;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
