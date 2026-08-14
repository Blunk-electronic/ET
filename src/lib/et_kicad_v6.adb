------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              KICAD V6                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your editor to 4.

with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;

package body et_kicad_v6 is


	function to_string (path : in pac_uuid_path.list) return string is
		result : ada.strings.unbounded.unbounded_string;

		procedure query_uuid (c : in pac_uuid_path.cursor) is begin
			ada.strings.unbounded.append (result, "/" & to_string (pac_uuid_path.element (c)));
		end query_uuid;
	begin
		path.iterate (query_uuid'access);
		return ada.strings.unbounded.to_string (result);
	end to_string;


	function to_uuid_path (text : in string) return pac_uuid_path.list is
		result : pac_uuid_path.list;
		start  : positive := text'first;
	begin
		if text'length = 0 then
			return result;
		end if;

		-- Skip a leading slash if present:
		if text (start) = '/' then
			start := start + 1;
		end if;

		declare
			remainder : constant string := text (start .. text'last);
			cursor_pos : natural := remainder'first;
			slash_pos  : natural;
		begin
			loop
				slash_pos := index (remainder (cursor_pos .. remainder'last), "/");

				if slash_pos = 0 then
					if cursor_pos <= remainder'last then
						result.append (to_uuid (remainder (cursor_pos .. remainder'last)));
					end if;
					exit;
				else
					result.append (to_uuid (remainder (cursor_pos .. slash_pos - 1)));
					cursor_pos := slash_pos + 1;
				end if;
			end loop;
		end;

		return result;
	end to_uuid_path;


	procedure log_unknown_key (
		context			: in string;
		key				: in string;
		log_threshold	: in type_log_level;
		deferred		: in boolean := false)
	is begin
		if deferred then
			log (SEVERITY_NOTE,
				"in " & context & ": key '" & key & "' is a known but not-yet-populated stub",
				level => log_threshold);
		else
			log (SEVERITY_WARNING,
				"in " & context & ": unrecognized key '" & key & "'",
				level => log_threshold);
		end if;
	end log_unknown_key;


end et_kicad_v6;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
