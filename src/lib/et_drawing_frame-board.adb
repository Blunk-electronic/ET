------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        DRAWING FRAME BOARD                               --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;


package body et_drawing_frame.board is
	

	procedure apply_defaults_board (frame : in out type_frame_pcb_pre) is

		-- LINES OF TITLE BLOCK
		type type_lines is array (positive range <>) of type_line;

		
		lines_pcb : constant type_lines (1 .. 8) := (
			-- outer lines
			((  0,  0),(220,  0)),
			((220,  0),(220, 50)),
			((220, 50),(  0, 50)),
			((  0, 50),(  0,  0)),

			-- inner lines
			(( 89,  0),( 89, 50)), -- vertical
			((119, 15),(119,  0)), -- vertical
			((150, 15),(150,  0)), -- vertical
			(( 89, 15),(220, 15)) -- horizontal
			--(( 89, 20),(220, 20)) -- horizontal
			);

		
		-- Collects the lines of the given array and returns them as a list:
		function make_lines (lines : in type_lines) return pac_lines.list is
			use pac_lines;
			result : pac_lines.list;
		begin
			for i in lines'first .. lines'last loop
				result.append (lines (i));
			end loop;
			return result;
		end make_lines;
		----------------------------------

		-- TEXTS IN TITLE BLOCK
		type type_texts is array (positive range <>) of type_static_text;

		
		texts_pcb : constant type_texts (1 .. 11) := (
			(position => (  2, 36), size => 3, content => to_content ("Company:")),
			(position => (  2, 31), size => 3, content => to_content ("Project:")),
			(position => (  2, 26), size => 3, content => to_content ("Module:")),
			(position => (  2, 21), size => 3, content => to_content ("Variant:")),
			(position => (  2, 16), size => 3, content => to_content ("Customer:")),
			(position => (  2, 11), size => 3, content => to_content ("Partcode:")),
			(position => (  2,  6), size => 3, content => to_content ("Drwg. No:")),
			(position => (  2,  1), size => 3, content => to_content ("Revision:")),

			--(position => (120, 16), size => 3, content => to_content ("date")),
			--(position => (152, 16), size => 3, content => to_content ("name")),
			(position => ( 90, 11), size => 3, content => to_content ("edited:")),
			(position => ( 90,  6), size => 3, content => to_content ("checked:")),
			(position => ( 90,  1), size => 3, content => to_content ("approved"))
			);

		
		-- Collects the texts of the given array and returns them as a list:
		function make_texts (texts : in type_texts) return pac_static_texts.list is
			use pac_static_texts;
			result : pac_static_texts.list;
		begin
			for i in texts'first .. texts'last loop
				result.append (texts (i));
			end loop;
			return result;
		end make_texts;
		
		
	begin -- apply_defaults_board

		-- type_title_bock (basic stuff):
		frame.title_block_pcb.position												:= ( 55,  6);
		frame.title_block_pcb.lines := make_lines (lines_pcb);
		frame.title_block_pcb.placeholders_common.project_name.position				:= ( 30, 31);
		frame.title_block_pcb.placeholders_common.module_file_name.position			:= ( 30, 26);
		frame.title_block_pcb.placeholders_common.active_assembly_variant.position	:= ( 30, 21);
		frame.title_block_pcb.static_texts := make_texts (texts_pcb);

		frame.title_block_pcb.placeholders_additional.company.position 				:= ( 30, 36);
		frame.title_block_pcb.placeholders_additional.customer.position 			:= ( 30, 16);
		frame.title_block_pcb.placeholders_additional.partcode.position 			:= ( 30, 11);
		frame.title_block_pcb.placeholders_additional.drawing_number.position		:= ( 30,  6);
		frame.title_block_pcb.placeholders_additional.revision.position 			:= ( 30,  1);

		frame.title_block_pcb.placeholders_additional.drawn_by.position 			:= (152, 11);
		frame.title_block_pcb.placeholders_additional.checked_by.position 			:= (152,  6);
		frame.title_block_pcb.placeholders_additional.approved_by.position 			:= (152,  1);

		frame.title_block_pcb.placeholders_additional.drawn_date.position 			:= (120, 11);
		frame.title_block_pcb.placeholders_additional.checked_date.position 		:= (120,  6);
		frame.title_block_pcb.placeholders_additional.approved_date.position 		:= (120,  1);

		frame.title_block_pcb.placeholders_additional.face.position 				:= (120, 46);
		frame.title_block_pcb.placeholders_additional.signal_layer.position 		:= (120, 41);

		-- cam markers
		frame.title_block_pcb.cam_markers.face.position 							:= ( 90, 46);
		frame.title_block_pcb.cam_markers.signal_layer.position 					:= ( 90, 41);
		
		frame.title_block_pcb.cam_markers.silk_screen.position 						:= (120, 36);
		frame.title_block_pcb.cam_markers.assy_doc.position 						:= (155, 36);
		frame.title_block_pcb.cam_markers.keepout.position 							:= (185, 36);
		
		frame.title_block_pcb.cam_markers.stop_mask.position 						:= (120, 31);
		frame.title_block_pcb.cam_markers.stencil.position 							:= (155, 31);

		frame.title_block_pcb.cam_markers.pcb_outline.position 						:= (120, 26);
		frame.title_block_pcb.cam_markers.plated_millings.position					:= (155, 26);

		frame.title_block_pcb.cam_markers.route_restrict.position 					:= (120, 21);
		frame.title_block_pcb.cam_markers.via_restrict.position 					:= (170, 21);

	end apply_defaults_board;



	
	function make_default_frame_pcb
		return type_frame_pcb_pre
	is
		f : type_frame_pcb_pre;
	begin
		apply_defaults_board (f);
		return f;
	end make_default_frame_pcb;
	
	
end et_drawing_frame.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
