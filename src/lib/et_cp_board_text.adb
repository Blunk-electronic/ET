------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   COMMAND PROCESSOR / BOARD / TEXT                       --
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
-- - rework
-- - propose arguments if command incomplete
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_directions;						use et_directions;
with et_primitive_objects;				use et_primitive_objects;
with et_pcb_sides;						use et_pcb_sides;

with et_exceptions;						use et_exceptions;
with et_keywords;						use et_keywords;

with et_board_layer_category;			use et_board_layer_category;
with et_pcb_signal_layers;

with et_board_geometry;					use et_board_geometry;

with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stopmask;
with et_board_ops.conductors;

with et_text_content;					use et_text_content;
with et_board_text;						use et_board_text;
with et_board_ops.text;					use et_board_ops.text;

with et_pcb_placeholders;
with et_pcb_placeholders.conductor;
with et_pcb_placeholders.non_conductor;


package body et_cp_board_text is

	use pac_generic_modules;
	use pac_geometry_2;

	use pac_text_board_vectorized;

	

	
	procedure place_text (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		text			: type_text_fab_with_content;
		pos_xy			: type_vector_model;
		rotation		: type_rotation_model;
		content			: pac_text_content.bounded_string;
		layer_category	: type_layer_category;
		
		face			: type_face;

		procedure place_in_assy_doc is
			use et_board_ops.assy_doc;
		begin
			add_text (
				module_cursor 	=> module,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_assy_doc;


		procedure place_in_silkscreen is
			use et_board_ops.silkscreen;
		begin
			add_text (
				module_cursor 	=> module,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_silkscreen;


		procedure place_in_stopmask is
			use et_board_ops.stopmask;
		begin
			add_text (
				module_cursor 	=> module,
				face			=> face,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_stopmask;


		procedure place_in_conductor_layer is
			use et_board_ops.conductors;
			use et_pcb_signal_layers;
			signal_layer	: type_signal_layer;
		begin
			signal_layer := to_signal_layer (get_field (cmd, 6));  -- 5 
							
			-- This procedure automatically cares for mirroring:
			add_text (
				module_cursor 	=> module,
				signal_layer	=> signal_layer,
				text			=> text,
				log_threshold	=> log_threshold + 1);
		end place_in_conductor_layer;
		
		
	begin
		-- CS log message
		
		-- board demo place text silkscreen top 0.15 1 140 100 0 "SILKSCREEN"
		-- board demo place text conductor  5   0.15 1 140 100 0 "L1"

		-- CS: argument for alignment

		
		case cmd_field_count is
			when 12 =>
				layer_category := to_layer_category (get_field (cmd, 5));
				text.line_width := to_distance (get_field (cmd, 7)); -- 0.15
				text.size := to_distance (get_field (cmd, 8)); -- 1
				
				pos_xy := to_vector_model (get_field (cmd, 9), get_field (cmd, 10));

				rotation := to_rotation (get_field (cmd, 11)); -- 0
				text.position := type_position (to_position (pos_xy, rotation));
				
				text.content := to_content (get_field (cmd, 12));
				-- CS check length

				
				if characters_valid (content) then

					case layer_category is
						when LAYER_CAT_ASSY =>
							face := to_face (get_field (cmd, 6)); -- top/bottom
							place_in_assy_doc;

							
						when LAYER_CAT_SILKSCREEN =>
							face := to_face (get_field (cmd, 6)); -- top/bottom
							place_in_silkscreen;

							
						when LAYER_CAT_STOPMASK =>						
							face := to_face (get_field (cmd, 6)); -- top/bottom
							place_in_stopmask;
							
						
						when LAYER_CAT_CONDUCTOR =>
							place_in_conductor_layer;
							

						when others => null; -- CS message invalid layer category ?
					end case;

				else
					raise syntax_error_1 with
						"ERROR: Invalid character in text !";
					-- CS show invalid character and its position
				end if;

				
			when 13 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end place_text;

		








	procedure place_text_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		use pac_text_board;
		use pac_text_board_vectorized;
		use et_pcb_placeholders;
		use et_pcb_placeholders.conductor;
		use et_pcb_placeholders.non_conductor;
		
		pos_xy			: type_vector_model;
		rotation		: type_rotation_model;
		size			: type_distance_positive;
		linewidth		: type_distance_positive;
		layer_category	: type_layer_category;
		face			: type_face;

		
		procedure place_in_assy_doc is
			use et_board_ops.assy_doc;
			ph : type_placeholder_non_conductor; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (cmd, 12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;
			
			add_placeholder (
				module_cursor 	=> module,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_assy_doc;



		procedure place_in_silkscreen is
			use et_board_ops.silkscreen;
			ph : type_placeholder_non_conductor; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (cmd, 12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;

			add_placeholder (
				module_cursor 	=> module,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_silkscreen;



		procedure place_in_stopmask is
			use et_board_ops.stopmask;
			ph : type_placeholder_non_conductor; -- non conductor layers
		begin
			ph.meaning := to_meaning (get_field (cmd, 12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;

			add_placeholder (
				module_cursor 	=> module,
				placeholder		=> ph,
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end place_in_stopmask;

		
		
		procedure place_in_conductor_layer is
			use et_board_ops.conductors;
			use et_pcb_signal_layers;
			ph : type_placeholder_conductor; -- conductor layers
		begin
			ph.layer := to_signal_layer (get_field (cmd, 6));  -- 5 
			ph.meaning := to_meaning (get_field (cmd, 12));
			ph.position := type_position (to_position (pos_xy, rotation));
			ph.line_width := linewidth;
			ph.size := size;
			
			-- This procedure automatically cares for mirroring:
			add_placeholder (
				module_cursor 	=> module,
				placeholder		=> ph,
				log_threshold	=> log_threshold + 1);

		end place_in_conductor_layer;


		
	begin
		-- CS log message
		
		-- board demo place placeholder silkscreen top 0.15 1 140 100 0 module
		-- board demo place placeholder conductor  5   0.15 1 140 100 0 module

		-- CS: argument for alignment

		case cmd_field_count is
			when 12 =>
				layer_category := to_layer_category (get_field (cmd, 5));

				-- Get the linewidth of the placeholder:
				linewidth := to_distance (get_field (cmd, 7)); -- 0.15
				validate_text_line_width (linewidth);

				-- Get the size of the placeholder:
				size := to_distance (get_field (cmd, 8)); -- 1
				validate_text_size (size);
				
				-- Get the position of the placeholder:
				pos_xy := to_vector_model (get_field (cmd, 9), get_field (cmd, 10));
				rotation := to_rotation (get_field (cmd, 11)); -- 0
				
				
				case layer_category is
					when LAYER_CAT_ASSY =>
						face := to_face (get_field (cmd, 6)); -- top/bottom
						place_in_assy_doc;


					when LAYER_CAT_SILKSCREEN =>
						face := to_face (get_field (cmd, 6)); -- top/bottom
						place_in_silkscreen;


					when LAYER_CAT_STOPMASK =>
						face := to_face (get_field (cmd, 6)); -- top/bottom
						place_in_stopmask;

					
					when LAYER_CAT_CONDUCTOR =>
						place_in_conductor_layer;


					when others => null; -- CS message invalid layer category ?
				end case;
					
			when 13 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end place_text_placeholder;




		
	
end et_cp_board_text;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
