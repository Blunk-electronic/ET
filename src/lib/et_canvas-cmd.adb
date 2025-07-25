------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS COMMANDS                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with ada.characters.handling;		use ada.characters.handling;

with ada.text_io;					use ada.text_io;
with et_string_processing;			use et_string_processing;
with et_modes;						use et_modes;
with et_keywords;					use et_keywords;
with et_exceptions;					use et_exceptions;
with et_logging;					use et_logging;
with et_command_processor;			use et_command_processor;


package body et_canvas.cmd is


	
	procedure skipped_in_this_runmode (
		log_threshold : in type_log_level) 
	is begin
		log (text => "skipped in current runmode "
			& to_string (runmode), level => log_threshold);
	end skipped_in_this_runmode;


	

	function to_string (
		noun : in type_canvas_noun)
		return string
	is begin
		return type_canvas_noun'image (noun);
	end to_string;

	

	


	procedure parse_canvas_command (
		cmd		: in out type_single_cmd;
		verb	: in type_canvas_verb;
		noun	: in type_canvas_noun)
	is

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- This procedure is a shortcut. 
		-- Call it in case the given command is too long:
		procedure too_long is begin
			command_too_long (cmd, cmd_field_count - 1);
		end;


		-- This procedure is a shortcut. 
		-- Call it in case the given command is incomplete:
		procedure command_incomplete is begin
			command_incomplete (cmd);
		end;


		

		function get_field (place : in type_field_count) 
			return string 
		is begin
			return get_field (cmd, place);
		end;

		


		
		
	
		procedure evaluate_on_verb_set is 

			-- This procedure parses a command that
			-- sets the grid spacing like
			-- "set grid spacing 5 5":
			procedure set_grid is begin				
				case cmd_field_count is

					when 5 =>
						if to_lower (get_field (5)) = keyword_on then
						-- schematic led_driver set grid on/off

							grid.on_off := ON;
							
						elsif to_lower (get_field (5)) = keyword_off then
							grid.on_off := OFF;
						end if;
						
							
					when 6 =>
						if to_lower (get_field (5)) = keyword_spacing then
						-- schematic led_driver set grid spacing 5
							
							grid.spacing := (
								x => to_distance (get_field (6)),
								y => to_distance (get_field (6)));

							set_grid_to_scale;
							update_grid_display;
							
							
						elsif to_lower (get_field (5)) = keyword_style then
						-- schematic led_driver set grid style dots/lines

							if to_lower (get_field (6)) = keyword_dots then
								grid.style := DOTS;
							elsif to_lower (get_field (6)) = keyword_lines then
								grid.style := LINES;
							end if;
							
						end if;

						
					when 7 =>
						if to_lower (get_field (5)) = keyword_spacing then
						-- schematic led_driver set grid spacing 5 5
							
							grid.spacing := (
								x => to_distance (get_field (6)),
								y => to_distance (get_field (7)));

							set_grid_to_scale;
							update_grid_display;
						end if;

						
					when 8 .. type_field_count'last => 
						too_long;

					
					when others =>  -- command incomplete
						command_incomplete;
						
				end case;
			end set_grid;

			
			-- Parses a command that sets the zoom
			-- factor like "set zoom 3":
			procedure set_zoom is 
				l : type_zoom_factor;
			begin
				case cmd_field_count is
					when 5 =>  -- set zoom 3
						l := to_zoom_factor (get_field (5));

						log (text => "set zoom factor " & to_string (l),
							level => log_threshold + 1);					

						zoom_to (get_cursor_position, l);

					when 6 .. type_field_count'last => too_long;

					when others => command_incomplete;
				end case;			
			end set_zoom;


			
			
			-- Parses a command that sets the cursor
			-- to a certain place like "set cursor 10 10":
			procedure set_cursor is 

				-- Sets the cursor to a certain place
				-- but leaves the zoom factor unchanged:
				procedure set is
					c : type_vector_model := type_vector_model (set (
						x => to_distance (get_field (5)),
						y => to_distance (get_field (6))));

				begin
					log (text => "set cursor at " & to_string (c),
						level => log_threshold + 1);
							
					zoom_to (c, S); -- zoom factor remains unchanged
				end set;



				-- Zooms on a given point and places the cursor
				-- at the given point:
				procedure zoom_to_point is
					c : type_vector_model := type_vector_model (set (
						x => to_distance (get_field (5)),
						y => to_distance (get_field (6))));

					l : type_zoom_factor := to_zoom_factor (get_field (7));
				begin
					log (text => "zoom to point " & to_string (c) 
						& " zoom factor" & to_string (l),
						level => log_threshold + 1);
							
					zoom_to (c, l);
				end zoom_to_point;

				
			begin
				case cmd_field_count is
					when 6 =>  -- set cursor 10 10
						set;

					when 7 =>  -- set cursor 10 10 0.5 
						zoom_to_point;

					when 8 .. type_field_count'last => too_long;

					when others => command_incomplete;
				end case;
			end set_cursor;

			


			-- Parses a command that sets the scale like
			-- "set scale 10":
			procedure set_scale is

				-- Sets the scale, the grid according to the new scale,
				-- updates the scale and grid display:
				procedure set is
					M_new : type_scale := type_scale'value (get_field (5));		
					-- CS do a proper range check. exception handler ?
				begin
					M := M_new;

					-- If the operator changes the scale, then
					-- a default grid spacing must be set first:
					grid.spacing.x := grid_spacing_default;
					grid.spacing.y := grid_spacing_default;
					
					-- If the operator changes the scale, then the
					-- visible grid spacing must be changed accordingly.
					set_grid_to_scale;
					
					-- put_line ("grid spacing x/y:" 
					-- 	& to_string (grid.spacing.x) & "/" & to_string (grid.spacing.y));

					update_grid_display;
					update_scale_display;
				end set;

				
			begin
				case cmd_field_count is
					when 5 =>  -- set scale 3
						set;

					when 6 .. type_field_count'last => too_long;

					when others => command_incomplete;
				end case;
			end set_scale;

			
			
		begin
			case noun is
				when NOUN_GRID =>
					set_grid;					
				
				when NOUN_ZOOM => 
					set_zoom;
									
				when NOUN_CURSOR =>
					set_cursor;

				when NOUN_SCALE =>
					set_scale;				
					
				when others => invalid_noun (to_string (noun));
			end case;
		end evaluate_on_verb_set;




		
		procedure evaluate_on_verb_move is 

			-- Parses a command that moves the cursor
			-- by a certain offset like "move cursor 5 -10":
			procedure move_cursor is

				procedure move is
					c : type_vector_model := type_vector_model (set (
						x => to_distance (get_field (5)),
						y => to_distance (get_field (6))));

				begin
					log (text => "move cursor by " & to_string (c),
						level => log_threshold + 1);
							
					move_cursor_by (c);
				end move;

				
			begin
				case cmd_field_count is
					when 6 =>  -- move cursor 5 -10
						move_cursor;

					when 7 .. type_field_count'last => too_long;

					when others => command_incomplete;
				end case;
			end move_cursor;
		

		begin
			case noun is
				when NOUN_CURSOR =>
					move_cursor;
				
				when others => invalid_noun (to_string (noun));
			end case;
		end evaluate_on_verb_move;


		
		
	begin
		log (text => "parse canvas command ...", level => log_threshold + 1);

		-- Canvas commands can only be executed 
		-- in a graphical runmode:
		case runmode is
			when MODE_MODULE =>
				
				-- Evaluate the given verb:
				case verb is
					when VERB_SET =>
						evaluate_on_verb_set;

					when VERB_MOVE =>
						evaluate_on_verb_move;

				end case;


		-- CS do something if command is incomplete and if it
		-- was executed as single command.
		-- like
		-- if not is_complete (cmd) then
		-- 	propose_arguments;
		-- end if;

			
			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;				
	end parse_canvas_command;

	
end et_canvas.cmd;

