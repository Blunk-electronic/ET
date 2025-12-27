------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE READ / TEXT IN BOARD                         --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_module_instance;			use et_module_instance;
with et_keywords;					use et_keywords;

with et_board_geometry;				use et_board_geometry;
with et_board_coordinates;			use et_board_coordinates;

with et_text_content;				use et_text_content;

with et_conductor_text;
with et_conductor_text.boards;

with et_board_text;					use et_board_text;
with et_alignment;					use et_alignment;

with et_pcb_placeholders;				use et_pcb_placeholders;
with et_pcb_placeholders.conductor;		use et_pcb_placeholders.conductor;
with et_pcb_placeholders.non_conductor;	use et_pcb_placeholders.non_conductor;

with et_pcb;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_mirroring;

with et_stopmask;
with et_silkscreen;
with et_assy_doc;

with et_board_ops;

with et_general_rw;					use et_general_rw;



package body et_module_read_text_board is

	use pac_generic_modules;
	use pac_geometry_2;	
	use pac_text_board_vectorized;



	board_text : type_text_fab_with_content;
	board_text_placeholder : type_placeholder_non_conductor;



	procedure read_board_text_placeholder (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			board_text_placeholder.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			board_text_placeholder.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text_placeholder.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_meaning then -- meaning project_name
			expect_field_count (line, 2);
			board_text_placeholder.meaning := to_meaning (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_placeholder;





	procedure read_board_text_non_conductor (
		line : in type_fields_of_line)
	is
		kw : constant  string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position starting at field 2
			board_text.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			board_text.line_width := to_distance (f (line, 2));

			-- CS validate against dru settings
			
		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
			expect_field_count (line, 2); -- actual content in quotes !
			board_text.content := to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;

	end read_board_text_non_conductor;


	

	
	
	-- This variable is used for vector texts in conductor layers
	-- and restrict layers:
	board_text_conductor : et_conductor_text.boards.type_conductor_text_board;

	
	
	procedure read_board_text_conductor (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position starting at field 2
			board_text_conductor.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text_conductor.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			board_text_conductor.line_width := to_distance (f (line, 2));

			-- CS validate against dru settings
			
		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text_conductor.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "TOP", "L2", "BOT"
			expect_field_count (line, 2); -- actual content in quotes !
			board_text_conductor.content := to_content (f (line, 2));

		elsif kw = keyword_layer then -- layer 15
			expect_field_count (line, 2);
			board_text_conductor.layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer (board_text_conductor.layer);
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_conductor;


	

	
	
	-- This variable is used for text placeholders in conductor layers:
	board_text_conductor_placeholder : type_placeholder_conductor;

	
	
	procedure read_board_text_conductor_placeholder (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			board_text_conductor_placeholder.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text_conductor_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			board_text_conductor_placeholder.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text_conductor_placeholder.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_meaning then -- meaning revision/project_name/...
			expect_field_count (line, 2);
			board_text_conductor_placeholder.meaning := to_meaning (f (line, 2));

		elsif kw = keyword_layer then -- layer 15
			expect_field_count (line, 2);
			board_text_conductor_placeholder.layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer (board_text_conductor_placeholder.layer);
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_conductor_placeholder;

	
	
	
	
	
		
	procedure read_board_text_contours (
		line : in type_fields_of_line)
	is
		kw : constant  string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position starting at field 2
			board_text.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			board_text.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
			expect_field_count (line, 2); -- actual content in quotes !
			board_text.content := to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_contours;

	
	
	
	
	
	
	procedure insert_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
	-- The board_text_placeholder has been a general thing until now. 
	-- Depending on the layer and the side of the board (face) the board_text_placeholder
	-- is now assigned to the board where it belongs to.
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_pcb_sides;
			use et_board_coordinates;
			use et_pcb;
			use et_board_text;
			use et_pcb_placeholders;
		begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.silkscreen.top.placeholders,
								new_item	=> board_text_placeholder);

						when LAYER_CAT_ASSY =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.assy_doc.top.placeholders,
								new_item	=> board_text_placeholder);

						when LAYER_CAT_STOPMASK =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.stopmask.top.placeholders,
								new_item	=> board_text_placeholder);

						-- CS
						--when KEEPOUT =>
						--	pac_placeholders_non_conductor.append (
						--		container	=> module.board.keepout.top.placeholders,
						--		new_item	=> board_text_placeholder);

						when others => invalid_section;
					end case;
					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.silkscreen.bottom.placeholders,
								new_item	=> board_text_placeholder);

						when LAYER_CAT_ASSY =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.assy_doc.bottom.placeholders,
								new_item	=> board_text_placeholder);
							
						when LAYER_CAT_STOPMASK =>
							pac_placeholders_non_conductor.append (
								container	=> module.board.stopmask.bottom.placeholders,
								new_item	=> board_text_placeholder);

						-- CS
						--when KEEPOUT =>
						--	pac_placeholders_non_conductor.append (
						--		container	=> module.board.keepout.bottom.placeholders,
						--		new_item	=> board_text_placeholder);

						when others => invalid_section;
					end case;
					
			end case;
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& "insert placeholder",
			level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board placeholder
		reset_placeholder (board_text_placeholder);		
		
		log_indentation_down;
	end insert_placeholder;
				
		
		
		
		
		
	procedure insert_board_text_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_placeholders_conductor.append (
				container	=> module.board.conductors_floating.placeholders,
				new_item	=> board_text_conductor_placeholder);
		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert_board_text_placeholder",
			level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next placeholder in conductor layer
		reset_placeholder (board_text_conductor_placeholder);
		
		log_indentation_down;
	end insert_board_text_placeholder;

		
		
		
		
		
		
	procedure build_non_conductor_text (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face 			: in et_pcb_sides.type_face;
		log_threshold	: in type_log_level)
	is
	-- The board_text has been a general thing until now. 
	-- Depending on the layer category and the side of the board (face) the board_text
	-- is now assigned to the board where it belongs to.
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_silkscreen;
			use et_assy_doc;
			use et_stopmask;

		begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							pac_silk_texts.append (
								container	=> module.board.silkscreen.top.texts,
								new_item	=> (board_text with null record));

						when LAYER_CAT_ASSY =>
							pac_doc_texts.append (
								container	=> module.board.assy_doc.top.texts,
								new_item	=> (board_text with null record));

						when LAYER_CAT_STOPMASK =>
							pac_stop_texts.append (
								container	=> module.board.stopmask.top.texts,
								new_item	=> (board_text with null record));

						when others => invalid_section;
					end case;
					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							pac_silk_texts.append (
								container	=> module.board.silkscreen.bottom.texts,
								new_item	=> (board_text with null record));

						when LAYER_CAT_ASSY =>
							pac_doc_texts.append (
								container	=> module.board.assy_doc.bottom.texts,
								new_item	=> (board_text with null record));

						when LAYER_CAT_STOPMASK =>
							pac_stop_texts.append (
								container	=> module.board.stopmask.bottom.texts,
								new_item	=> (board_text with null record));

						when others => invalid_section;
					end case;
					
			end case;
		end do_it;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " build non-conductor text",
			level => log_threshold);
			
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
			
		-- clean up for next board text
		reset_text (board_text);
		
		log_indentation_down;
	end build_non_conductor_text;

		
		
		
		
		

		
	procedure build_conductor_text (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)		
	is
		
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_pcb;
			use et_conductor_text.boards;
			use pac_conductor_texts;
			use et_board_ops;
			use et_mirroring;
			
			mirror : type_mirror;
			
		begin
			mirror := signal_layer_to_mirror (board_text_conductor.layer, get_deepest_conductor_layer (module_cursor));

			-- vectorize the text:
			board_text_conductor.vectors := vectorize_text (
				content			=> board_text_conductor.content,
				size			=> board_text_conductor.size,
				rotation		=> get_rotation (board_text_conductor.position),
				position		=> board_text_conductor.position.place,
				mirror			=> mirror,
				line_width		=> board_text_conductor.line_width,
				make_border		=> true,
				log_threshold	=> log_threshold + 2
				-- CS alignment
				); 

			append (
				container	=> module.board.conductors_floating.texts,
				new_item	=> board_text_conductor);

		end do_it;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& "build conductor text",
			level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next text in conductor layer
		et_conductor_text.boards.reset_text (board_text_conductor);
		
		log_indentation_down;
	end build_conductor_text;

				
				
end et_module_read_text_board;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
