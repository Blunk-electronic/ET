------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / TEXT                            --
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

with et_meta;

with et_assembly_variants;
with et_conductor_text.boards;
with et_mirroring;



package body et_board_ops.text is

	use pac_devices_sch;
	use pac_devices_non_electric;
	use pac_nets;



	-- Maps from the meaning of a text to its actutal content.
	function to_placeholder_content (
		module_cursor	: in pac_generic_modules.cursor;
		meaning 		: in type_text_meaning)
		return et_text.pac_text_content.bounded_string 
	is
		m : type_generic_module renames element (module_cursor);
		
		use et_text;

		use et_meta;
		meta : constant et_meta.type_board := m.meta.board;

		use et_assembly_variants;
		use pac_assembly_variant_name;
		variant : constant pac_assembly_variant_name.bounded_string := m.active_variant;

		result : pac_text_content.bounded_string;

		use et_pcb;
	begin
		case meaning is
			when COMPANY			=> result := to_content (to_string (meta.company));
			when CUSTOMER			=> result := to_content (to_string (meta.customer));
			when PARTCODE			=> result := to_content (to_string (meta.partcode));
			when DRAWING_NUMBER		=> result := to_content (to_string (meta.drawing_number));
			when ASSEMBLY_VARIANT	=> result := to_content (to_string (variant));
			when PROJECT			=> result := to_content ("not assigned"); -- CS
			when MODULE				=> result := to_content (to_string (key (module_cursor)));
			when REVISION			=> result := to_content (to_string (meta.revision));
		end case;
		
		return result;
	end to_placeholder_content;



	
	
	procedure place_text_in_non_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category;
		face			: in type_face; -- top/bottom
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is 
		use et_silkscreen;
		use et_assy_doc;
		use et_stopmask;

		text_tmp : type_text_fab_with_content := text;
		
		
		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_text;
			use pac_doc_texts;
			use pac_silk_texts;
			use pac_stop_texts;
			use et_mirroring;

			v_text : type_vector_text;		
			mirror : type_mirror;
		begin
			mirror := face_to_mirror (face);
			
			v_text := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (text.position),
				position	=> text.position.place,
				mirror		=> mirror,
				line_width	=> text.line_width
				-- CS alignment
				); 
			
			case layer_category is
				when LAYER_CAT_ASSY =>
					case face is
						when TOP =>
							-- append (module.board.assy_doc.top.texts, (text with v_text));
							text_tmp.mirror := MIRROR_NO;
							append (module.board.assy_doc.top.texts, (text_tmp with null record));
						when BOTTOM =>
							-- append (module.board.assy_doc.bottom.texts, (text with v_text));
							text_tmp.mirror := MIRROR_ALONG_Y_AXIS;
							append (module.board.assy_doc.bottom.texts, (text_tmp with null record));
					end case;

				when LAYER_CAT_SILKSCREEN =>
					case face is
						when TOP =>
							append (module.board.silkscreen.top.texts, (text with v_text));
						when BOTTOM =>
							append (module.board.silkscreen.bottom.texts, (text with v_text));
					end case;
					
				when LAYER_CAT_STOP =>
					case face is
						when TOP =>
							append (module.board.stop_mask.top.texts, (text with v_text));
						when BOTTOM =>
							append (module.board.stop_mask.bottom.texts, (text with v_text));
					end case;

				when others => null;
			end case;
		end place_text;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " placing text in non-conductor layer at" -- CS output category
			& to_string (text.position)
			& " face" & to_string (face),
			level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> place_text'access);

	end place_text_in_non_conductor_layer;

	

end et_board_ops.text;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
