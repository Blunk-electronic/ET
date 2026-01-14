------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / BOARD / LAYER DISPLAY                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- To Do:
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_pcb_sides;						use et_pcb_sides;
with et_pcb_signal_layers;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_generic_modules;				use et_generic_modules;
with et_modes.board;					use et_modes.board;

with et_keywords;


package body et_cp_board_display is

	

	procedure display_outline (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure display (
			status	: in string := "") 
		is 
			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			log (text => "display outline" & space & to_string (ls),
					level => log_threshold + 1);

			layers.outline := ls;
			
			-- CS exception handler if status is invalid
		end display;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 4 => display; -- if status is omitted
			
			when 5 => display (get_field (cmd, 5));
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_outline;


	




	procedure display_ratsnest (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure display (
			status	: in string := "") 
		is 
			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			log (text => "display ratsnest" & space & to_string (ls),
					level => log_threshold + 1);

			layers.ratsnest := ls;
			
			-- CS exception handler if status is invalid
		end display;
		
		
	begin
		-- CS log message
		case cmd_field_count is
			when 4 => display; -- if status is omitted
			
			when 5 => display (get_field (cmd, 5));
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_ratsnest;

		





	

	procedure display_non_conductor (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure display (
			layer	: in type_noun;
			face	: in string; -- top/bottom
			status	: in string := "") 
		is
			ls : type_layer_status;
			fc : type_face;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given face to type_face:
			fc := to_face (face);
			
			log (text => "display " & to_lower (to_string (layer)) 
					& space & to_string (ls),
					level => log_threshold + 1);

			-- CS: use commands like enable_silkscreen (top) ? (see et_display.board)

			case fc is
				when TOP =>
					case layer is
						when NOUN_SILKSCREEN 	=> layers.silkscreen.top		:= ls;
						when NOUN_ASSY			=> layers.assy_doc.top			:= ls;
						when NOUN_KEEPOUT		=> layers.keepout.top			:= ls;
						when NOUN_STENCIL		=> layers.stencil.top			:= ls;
						when NOUN_STOPMASK		=> layers.stop_mask.top			:= ls;
						when NOUN_ORIGINS		=> layers.device_origins.top	:= ls;
						
						when others => 
							log (importance => ERROR, text => "invalid layer !", console => true);
					end case;

				when BOTTOM =>
					case layer is
						when NOUN_SILKSCREEN 	=> layers.silkscreen.bottom		:= ls;
						when NOUN_ASSY			=> layers.assy_doc.bottom		:= ls;
						when NOUN_KEEPOUT		=> layers.keepout.bottom		:= ls;
						when NOUN_STENCIL		=> layers.stencil.bottom		:= ls;
						when NOUN_STOPMASK		=> layers.stop_mask.bottom		:= ls;
						when NOUN_ORIGINS		=> layers.device_origins.bottom	:= ls;
						
						when others => 
							log (importance => ERROR, text => "invalid layer !", console => true);
					end case;
			end case;
			
			-- CS exception handler if status is invalid
		end display;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 5 => display (noun, get_field (cmd, 5)); -- if status is omitted
			
			when 6 => display (noun, get_field (cmd, 5), get_field (cmd, 6));
			
			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_non_conductor;

		
	


	


	procedure display_conductor (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_pcb_signal_layers;
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure display (
			layer	: in string;
			status	: in string := "")
		is 
			ls : type_layer_status;
			ly : type_signal_layer;
		begin			
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			ly := to_signal_layer (layer);
			
			log (text => "display conductor layer " & to_string (ly) & space & to_string (ls),
					level => log_threshold + 1);

			layers.conductors (ly) := ls;
			
			-- CS exception handler if status is invalid
		end display;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 5 => display (get_field (cmd, 5)); -- if status is omitted
			
			when 6 => display (get_field (cmd, 5), get_field (cmd, 6));
			
			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_conductor;





	

	procedure display_vias (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure display_vias (
			status	: in string := "") 
		is 
			ls : type_layer_status;
			--ly : type_signal_layer;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			--ly := to_signal_layer (layer);
			
			--log (text => "display via layer " & to_string (ly) & space & to_string (ls),
			log (text => "display via layer " & space & to_string (ls),
					level => log_threshold + 1);

			--layers.vias (ly) := ls;
			layers.vias := ls;
			
			-- CS exception handler if status is invalid
		end display_vias;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			--when 5 => display_vias (get_field (5)); -- if status is omitted
			--when 6 => display_vias (get_field (5), get_field (6));
			--when 7 .. type_field_count'last => too_long;
			when 4 => display_vias; -- if status is omitted
			
			when 5 => display_vias (get_field (cmd, 5));
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_vias;

		





	

	procedure display_restrict (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_pcb_signal_layers;
		use et_keywords;
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);



		procedure display_restrict_layer (
			objects	: in string; -- route/via
			layer	: in string; -- 1, 2, 8, ...
			status	: in string := "")
		is 
			ls : type_layer_status;
			ly : type_signal_layer;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;

			-- Convert the given layer to type_signal_layer:
			ly := to_signal_layer (layer);
			
			if objects = keyword_route then
				log (text => "display route restrict layer " & to_string (ly) & space & to_string (ls),
					level => log_threshold + 1);

				layers.route_restrict (ly) := ls;
				
			elsif objects = keyword_via then
				log (text => "display via restrict layer " & to_string (ly) & space & to_string (ls),
					level => log_threshold + 1);

				layers.via_restrict (ly) := ls;
				
			else
				log (importance => ERROR, 
						text => "Expect keyword " &
						enclose_in_quotes (keyword_route) & " or " &
						enclose_in_quotes (keyword_via) & "after noun " &
						to_string (NOUN_RESTRICT) & " !",
						console => true);
				raise constraint_error;
			end if;
			
			-- CS exception handler if status is invalid
		end display_restrict_layer;
		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 6 => display_restrict_layer (get_field (cmd, 5), get_field (cmd, 6)); -- if status is omitted
			
			when 7 => display_restrict_layer (get_field (cmd, 5), get_field (cmd, 6), get_field (cmd, 7));

			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display_restrict;

	
end et_cp_board_display;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
