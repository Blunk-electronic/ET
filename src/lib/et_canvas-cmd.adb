------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS COMMANDS                                --
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

with ada.characters.handling;		use ada.characters.handling;
with et_modes;						use et_modes;
with et_keywords;					use et_keywords;
with et_exceptions;					use et_exceptions;



package body et_canvas.cmd is


	-- This function is a shortcut to get a single field
	-- from the current command:
	function f (place : in type_field_count) 
		return string 
	is begin
		return get_field (single_cmd_status.cmd, place);
	end;


	procedure command_too_long (
		cmd		: in type_fields_of_line;
		from	: in type_field_count) 
	is begin
		log (WARNING, "command " & enclose_in_quotes (to_string (cmd)) 
			 & " too long !",
			 console => true);
		
		log (text => " -> Excessive arguments after no." 
			 & type_field_count'image (from) & " ignored !",
			 console => true);
	end;


	

	-- This procedure is a shortcut. Call it in case
	-- the given command is too long:
	procedure too_long is begin
		command_too_long (single_cmd_status.cmd, cmd_field_count - 1);
	end;

	
	procedure command_incomplete is begin
		if runmode /= MODE_HEADLESS and cmd_entry_mode = SINGLE_CMD then
			single_cmd_status.complete := false;
		else
			raise exception_command_incomplete with "command not complete";
		end if;
	end command_incomplete;



	procedure invalid_noun (
		noun : in string) 
	is begin
		raise semantic_error_1 with
			"ERROR: Noun " & enclose_in_quotes (noun) 
			& " invalid for this operation !";
	end invalid_noun;


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
		verb	: in type_canvas_verb;
		noun	: in type_canvas_noun)
	is
		
		-- Zooms on the current cursor position:
		procedure set_zoom is 
			l : type_zoom_factor := to_zoom_factor (f (5));
		begin
			log (text => "set zoom factor " & to_string (l),
				level => log_threshold + 1);					

			zoom_to (get_cursor_position, l);
		end set_zoom;


		-- Zooms on a given point and places the cursor
		-- at the given point:
		procedure zoom_to_point is
			c : type_vector_model := type_vector_model (set (
				x => to_distance (f (5)),
				y => to_distance (f (6))));

			l : type_zoom_factor := to_zoom_factor (f (7));
		begin
			log (text => "zoom to point " & to_string (c) 
				& " zoom factor" & to_string (l),
				level => log_threshold + 1);
					
			zoom_to (c, l);
		end zoom_to_point;
		

		-- Sets the cursor at a given place:
		procedure set_cursor is
			c : type_vector_model := type_vector_model (set (
				x => to_distance (f (5)),
				y => to_distance (f (6))));

		begin
			log (text => "zoom to point " & to_string (c),
				level => log_threshold + 1);
					
			zoom_to (c, S); -- zoom factor remains unchanged
		end set_cursor;


		-- Sets the scale, the grid according to the new scale,
		-- updates the scale and grid display:
		procedure set_scale is
			M_new : type_scale := type_scale'value (f (5));		
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
		end set_scale;


		procedure move_cursor is
			c : type_vector_model := type_vector_model (set (
				x => to_distance (f (5)),
				y => to_distance (f (6))));

		begin
			log (text => "move cursor by " & to_string (c),
				level => log_threshold + 1);
					
			move_cursor_by (c);
		end move_cursor;

		
		
	
		procedure evaluate_on_verb_set is begin					
			case noun is

				when NOUN_GRID =>
					case cmd_field_count is

						when 5 =>
							if to_lower (f (5)) = keyword_on then
							-- schematic led_driver set grid on/off

								grid.on_off := ON;
								
							elsif to_lower (f (5)) = keyword_off then
								grid.on_off := OFF;
							end if;
							
								
						when 6 =>
							if to_lower (f (5)) = keyword_spacing then
							-- schematic led_driver set grid spacing 5
								
								grid.spacing := (
									x => to_distance (f (6)),
									y => to_distance (f (6)));

								set_grid_to_scale;
								update_grid_display;
								
								
							elsif to_lower (f (5)) = keyword_style then
							-- schematic led_driver set grid style dots/lines

								if to_lower (f (6)) = keyword_dots then
									grid.style := DOTS;
								elsif to_lower (f (6)) = keyword_lines then
									grid.style := LINES;
								end if;
								
							end if;

							
						when 7 =>
							if to_lower (f (5)) = keyword_spacing then
							-- schematic led_driver set grid spacing 5 5
								
								grid.spacing := (
									x => to_distance (f (6)),
									y => to_distance (f (7)));

								set_grid_to_scale;
								update_grid_display;
							end if;

							
						when 8 .. type_field_count'last => too_long;

						
						when others => command_incomplete;
					end case;
					
					
				
				when NOUN_ZOOM => 
					case cmd_field_count is
						when 5 =>  -- set zoom 3
							set_zoom;

						when 6 .. type_field_count'last => too_long;

						when others => command_incomplete;
					end case;


					
				when NOUN_CURSOR =>
					case cmd_field_count is
						when 6 =>  -- set cursor 10 10
							set_cursor;

						when 7 =>  -- set cursor 10 10 0.5 
							zoom_to_point;

						when 8 .. type_field_count'last => too_long;

						when others => command_incomplete;
					end case;

					

				when NOUN_SCALE => 
					case cmd_field_count is
						when 5 =>  -- set scale 3
							set_scale;

						when 6 .. type_field_count'last => too_long;

						when others => command_incomplete;
					end case;

					
				when others => invalid_noun (to_string (noun));
			end case;
		end evaluate_on_verb_set;


		
		procedure evaluate_on_verb_move is begin
			case noun is
				when NOUN_CURSOR =>
					case cmd_field_count is
						when 6 =>  -- move cursor 5 -10
							move_cursor;

						when 7 .. type_field_count'last => too_long;

						when others => command_incomplete;
					end case;

				
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

			
			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;				
	end parse_canvas_command;

	
end et_canvas.cmd;

