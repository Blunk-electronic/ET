------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE WRITE / TEXT IN BOARD                        --
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

with et_module;						use et_module;
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

with et_module_board;				use et_module_board;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_mirroring;

with et_stopmask;
with et_silkscreen;
with et_assy_doc;

with et_board_ops;

with et_file_write;					use et_file_write;
with et_file_sections;				use et_file_sections;
with et_coordinates_formatting;		use et_coordinates_formatting;


package body et_module_write_text_board is

	use pac_generic_modules;
	use pac_geometry_2;	
	-- use pac_text_board_vectorized;


	
	procedure write_texts_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_conductor_text.boards;
		use pac_conductor_texts_board;
		

		procedure write_text (c : in pac_conductor_texts_board.cursor) is 
			text : et_conductor_text.boards.type_conductor_text_board 
				renames element (c);
		begin
			section_mark (section_text, HEADER);

			write (keyword => keyword_content, wrap => true,
				parameters => to_string (text.content));

			write (keyword => keyword_position, parameters => 
				to_string (get_position (text), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (text.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (text.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (text.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (text.alignment.vertical));

			-- CS use et_alignment.to_string 

			write (keyword => keyword_layer, 
				parameters => to_string (text.layer));

			section_mark (section_text, FOOTER);
		end write_text;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			texts : pac_conductor_texts_board.list 
				renames module.board.conductors_floating.texts;
		begin
			iterate (texts, write_text'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write texts in conductor layers",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_texts_conductor;


	
		

	


	procedure write_texts_non_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face 			: in type_face;
		log_threshold	: in type_log_level)
	is
		use et_silkscreen;
		use pac_silk_texts;
		
		use et_assy_doc;
		use pac_doc_texts;
		
		use et_stopmask;
		use pac_stop_texts;

		
		procedure write_text (cursor : in pac_silk_texts.cursor) is 
			text : type_silk_text renames element (cursor);
		begin
			section_mark (section_text, HEADER);

			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));

			write (keyword => keyword_position, parameters => 
				to_string (get_position (text), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (text.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (text.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (text.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (text.alignment.vertical));

			-- CS use et_alignment.to_string 

			section_mark (section_text, FOOTER);
		end write_text;

		
		
		procedure write_text (cursor : in pac_doc_texts.cursor) is 
			text : type_doc_text renames element (cursor);
		begin
			section_mark (section_text, HEADER);

			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));

			write (keyword => keyword_position, parameters => 
				to_string (get_position (text), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (text.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (text.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (text.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (text.alignment.vertical));

			-- CS use et_alignment.to_string 

			section_mark (section_text, FOOTER);
		end write_text;
				
				
		
		procedure write_text (cursor : in pac_stop_texts.cursor) is 
			text : type_stop_text renames element (cursor);
		begin
			section_mark (section_text, HEADER);

			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));

			write (keyword => keyword_position, parameters => 
				to_string (get_position (text), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (text.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (text.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (text.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (text.alignment.vertical));

			-- CS use et_alignment.to_string 

			section_mark (section_text, FOOTER);
		end write_text;

		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.top.texts,
								write_text'access);

						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.top.texts,
								write_text'access);

						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.top.texts,
								write_text'access);
							
						when others => invalid_section;
					end case;

					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.bottom.texts,
								write_text'access);

						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.bottom.texts,
								write_text'access);

						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.bottom.texts,
								write_text'access);
							
						when others => invalid_section;
					end case;					
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write texts in non-conductor layers",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_texts_non_conductor;



	

	


	procedure write_placeholders_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_placeholders_conductor;
		

		procedure write_placeholder (
			cursor : in pac_placeholders_conductor.cursor) 
		is 
			ph : type_placeholder_conductor renames element (cursor);
		begin
			section_mark (section_placeholder, HEADER);
			
			write (keyword => keyword_meaning, 
				parameters => to_string (ph.meaning));
			
			---
			write (keyword => keyword_position, parameters => 
				to_string (get_position (ph), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (ph.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (ph.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (ph.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (ph.alignment.vertical));

			-- CS use et_alignment.to_string 			
			---
			
			write (keyword => keyword_layer, 
				parameters => to_string (ph.layer));
			section_mark (section_placeholder, FOOTER);
		end write_placeholder;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			texts : pac_placeholders_conductor.list 
				renames module.board.conductors_floating.placeholders;
		begin
			iterate (texts, write_placeholder'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write placeholders in conductor layers",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_placeholders_conductor;

	






	
	procedure write_placeholders_non_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face 			: in type_face;
		log_threshold	: in type_log_level)
	is

		use et_pcb_placeholders.non_conductor;
		use pac_placeholders_non_conductor;

		
		procedure write_placeholder (
			cursor : in pac_placeholders_non_conductor.cursor) 
		is
			ph : type_placeholder_non_conductor renames element (cursor);
		begin
			section_mark (section_placeholder, HEADER);
			
			write (keyword => keyword_meaning, 
				parameters => to_string (ph.meaning));
			
			---
			write (keyword => keyword_position, parameters => 
				to_string (get_position (ph), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (ph.size)); -- size 1.000
			
			write (keyword => keyword_linewidth, 
				parameters => to_string (ph.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (ph.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (ph.alignment.vertical));

			-- CS use et_alignment.to_string 			
			---
			
			section_mark (section_placeholder, FOOTER);
		end write_placeholder;

		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			case face is
				when TOP =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.top.placeholders,
								write_placeholder'access);

						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.top.placeholders,
								write_placeholder'access);

						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.top.placeholders,
								write_placeholder'access);
							
						when others => invalid_section;
					end case;

					
				when BOTTOM =>
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							iterate (module.board.silkscreen.bottom.placeholders,
								write_placeholder'access);

						when LAYER_CAT_ASSY =>
							iterate (module.board.assy_doc.bottom.placeholders,
								write_placeholder'access);

						when LAYER_CAT_STOPMASK =>
							iterate (module.board.stopmask.bottom.placeholders,
								write_placeholder'access);
							
						when others => invalid_section;
					end case;					
			end case;
		end query_module;

		

	begin
		log (text => "module " & to_string (module_cursor)
			 & " write placeholders in non-conductor layers",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_placeholders_non_conductor;



	

	
end et_module_write_text_board;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
