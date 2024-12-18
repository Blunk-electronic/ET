------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / KEEPOUT                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 



package body et_board_ops.keepout is


	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_keepout_zone;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		-- When searching among already existing zones then
		-- this flag is used to abort the iteration prematurely:
		proceed : boolean := true;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_keepout_zones;
			c : pac_keepout_zones.cursor;

			-- This procedure tests whether the candidate
			-- zone z is open. If z is open, then it tries
			-- to merge the given zone into z. If the merge operation
			-- succeedes then no more zones are iterated (flag proceed):
			procedure query_zone (z : in out type_keepout_zone) is
				use et_board_shapes_and_text;
				use pac_contours;
				mr : type_merge_result;
			begin
				-- put_line ("query_zone");
				if is_open (zone) then
					--put_line (" is open");
					merge_contours (z, zone, mr);
					if mr.successful then
						--put_line ("  successful");
						-- No more searching needed -> abort iterator
						proceed := false;
					end if;
				end if;
			end query_zone;

			
		begin
			case face is
				when TOP =>
					-- Iterate through the already existing zones:
					c := module.board.keepout.top.zones.first;

					while c /= pac_keepout_zones.no_element and proceed loop
						module.board.keepout.top.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.keepout.top.zones.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.keepout.bottom.zones.first;

					while c /= pac_keepout_zones.no_element and proceed loop
						module.board.keepout.bottom.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.keepout.bottom.zones.append (zone);
					end if;

			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & " drawing keepout zone" 
			 & to_string (face)
			 & " " & to_string (contour => zone, full => true),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end draw_zone;

	
	
end et_board_ops.keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
