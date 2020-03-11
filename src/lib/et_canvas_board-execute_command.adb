------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      BOARD CANVAS COMMAND EXECUTOR                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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


with ada.text_io;				use ada.text_io;
with ada.containers;			use ada.containers;
with ada.exceptions;			use ada.exceptions;
with scripting;					use scripting;
with et_devices;

separate (et_canvas_board)

procedure execute_command (
	self			: not null access type_view;
	cmd				: in type_fields_of_line;
	log_threshold	: in type_log_level) is

	function f (place : in positive) return string is begin
		return et_string_processing.field (cmd, place);
	end;

	function fields return count_type is begin
		return et_string_processing.field_count (cmd);
	end;

	procedure too_long is begin
		command_too_long (cmd, fields - 1);
	end;
		
	-- exit_code : type_exit_code := SUCCESSFUL; -- CS currently no need

	verb : type_verb_canvas;
	noun : type_noun_canvas;

	procedure zoom_center is
		-- Build the center point:
		c : type_point := type_point (set (
				x => to_distance (f (3)),
				y => to_distance (f (4))));
	begin
		log (text => "center on point", level => log_threshold + 1);
		center_on (canvas, c);
	end zoom_center;

	procedure set_scale (scale : in string) is -- CS should be percent of scale_to_fit
		s : gdouble := gdouble'value (scale);
	begin
		log (text => "zoom level", level => log_threshold + 1);
		set_scale (canvas, s);
	end set_scale;

	-- CS unify procedures show_unit and show_first_unit. They differ only in 
	-- the way the unit_name is assigned.

	procedure position_cursor is
-- 		coordinates : schematic_ops.type_coordinates;
	begin
		null;
	end position_cursor;		
	
	
begin -- execute_command
	log (text => "full command: " & enclose_in_quotes (to_string (cmd)), level => log_threshold);

	-- There must be at least 2 fields in the command:
	if fields >= 2 then
		verb := to_verb (f (1));
		noun := to_noun (f (2));
		
		case verb is
			when VERB_DISPLAY => null;

			when VERB_POSITION =>
				case noun is 
					when NOUN_CURSOR =>
						case fields is
							when 5 => position_cursor; -- position cursor absolute/relative 25 30
							when 6 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;

					when others => invalid_noun (to_string (noun));
				end case;
			
			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						case fields is
							when 3 => null;
							when 4 .. count_type'last => too_long;
							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
			when VERB_ZOOM =>
				case noun is
					when NOUN_FIT => -- zoom fit
						case fields is
							when 2 => 
								log (text => "zoom to fit", level => log_threshold + 1);
								scale_to_fit (canvas);

							when 3 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;

					when NOUN_LEVEL => -- zoom level 3
						case fields is
							when 3 => 
								set_scale (f (3));

							when 4 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when NOUN_CENTER => -- zoom center 10 10
						case fields is
							when 4 =>  -- zoom center 10 10
								zoom_center;

							when 5 =>  -- zoom center 10 10 0.5
								zoom_center;
								set_scale (f (5));

							when 6 .. count_type'last => too_long;

							when others => command_incomplete (cmd);
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;
			
		end case;

	else
		command_incomplete (cmd);
	end if;

	
	exception when event: others => 
	
		log (ERROR, "canvas command '" &
			to_string (cmd) & "' invalid !", console => true);

		log (text => ada.exceptions.exception_information (event), console => true);		

	
end execute_command;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
