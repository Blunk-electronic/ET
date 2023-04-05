-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DATE AND TIME                                --
--                                                                          --
--                               S p e c                                    --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings.maps;			use ada.strings.maps;
with ada.text_io;				use ada.text_io;

with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

with gnat.calendar;


package et_time is


	date_characters : character_set := to_set (span => ('0','9')) or to_set ("-:T");
	type type_date is new string (1..19); -- "2017-08-17T14:17:25"

	
	-- Returns the given date as string.
	function to_string (date : in type_date) return string;

	
	-- Returns true if given date is valid and plausible.
	function date_valid (date : in type_date) return boolean;

	
	-- Returns the current date as string in the format YYYY-MM-DDTHH:MM:SS
	function get_date (
		preamble : in boolean := true) 
		return string;


	-- Converts the given time to a string like "2017-08-17T14:17:25"
	function to_string_full (
		time	: ada.calendar.time)
		return string;

	-- Converts the given time to a string like "2017-08-17"
	function to_string_YMD (
		time	: ada.calendar.time)
		return string;

	
	-- Use it to indicate uninialized date.
	function date_first return time; -- returns 1901-01-01

	
	-- Converts something like 1901-01-01 to a time type:
	function to_date (
		date : in string) 
		return time;
		
end et_time;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
