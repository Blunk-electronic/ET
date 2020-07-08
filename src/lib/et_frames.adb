------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

package body et_frames is

	function to_paper_size (paper_size : in string) return type_paper_size is begin
		return type_paper_size'value (paper_size);
	end;
	
	function to_string (paper_size : in type_paper_size) return string is begin
		return type_paper_size'image (paper_size);
	end;

	
	function to_string (orientation : in type_orientation) return string is begin
		return to_lower (type_orientation'image (orientation));
	end;

	function to_orientation (orientation : in string) return type_orientation is begin
		return type_orientation'value (orientation);
	end;


	function to_string (rows : in type_rows) return string is begin
		return trim (type_rows'image (rows), left);
	end;

	function to_rows (rows : in string) return type_rows is begin
		return type_rows'value (rows);
	end;

	function to_string (columns : in type_columns) return string is begin
		return trim (type_columns'image (columns), left);
	end;

	function to_columns (columns : in string) return type_columns is begin
		return type_columns'value (columns);
	end;


	

	function to_string (distance : in type_distance) return string is begin
		return trim (type_distance'image (distance), left);
	end;

	function to_distance (distance : in string) return type_distance is begin
		return type_distance'value (distance);
	end;

	

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the corresponding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance is

		dimension : type_distance;
	
	begin
		case orientation is
			when LANDSCAPE =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_x;
							when Y => dimension := paper_size_A3_y;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_x;
							when Y => dimension := paper_size_A4_y;
						end case;
				end case;

			when PORTRAIT =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_y;
							when Y => dimension := paper_size_A3_x;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_y;
							when Y => dimension := paper_size_A4_x;
						end case;
				end case;

		end case;

		return dimension;
	end paper_dimension;




	function to_string (name : in pac_template_name.bounded_string) return string is begin
		return pac_template_name.to_string (name);
	end;
	
	function to_template_name (name : in string) return pac_template_name.bounded_string is begin
		return pac_template_name.to_bounded_string (name);
	end;

	function to_string (domain : in type_domain) return string is begin
		return to_lower (type_domain'image (domain));
	end;

	function to_domain (domain : in string) return type_domain is begin
		return type_domain'value (domain);
	end;

	procedure apply_defaults (frame : in out type_frame) is

		-- LINES OF TITLE BLOCK
		type type_lines is array (positive range <>) of type_line;

		lines_sch : constant type_lines (1 .. 10) := (
			-- outer lines
			((  0,  0),(220,  0)),
			((220,  0),(220, 40)),
			((220, 40),(  0, 40)),
			((  0, 40),(  0,  0)),

			-- inner lines
			(( 89,  0),( 89, 40)), -- vertical
			((119, 20),(119,  0)), -- vertical
			((150, 20),(150,  0)), -- vertical
			(( 89, 15),(220, 15)), -- horizontal
			(( 89, 20),(220, 20)), -- horizontal
			(( 89, 25),(220, 25))  -- horizontal
			);
		
		lines_pcb : constant type_lines (1 .. 9) := (
			-- outer lines
			((  0,  0),(220,  0)),
			((220,  0),(220, 40)),
			((220, 40),(  0, 40)),
			((  0, 40),(  0,  0)),

			-- inner lines
			(( 89,  0),( 89, 40)), -- vertical
			((119, 20),(119,  0)), -- vertical
			((150, 20),(150,  0)), -- vertical
			(( 89, 15),(220, 15)), -- horizontal
			(( 89, 20),(220, 20)) -- horizontal
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
		type type_texts is array (positive range <>) of type_text;

		texts_sch : constant type_texts (1 .. 16) := (
			(position => (  2, 36), size => 3, content => et_text.to_content ("Company:")),
			(position => (  2, 31), size => 3, content => et_text.to_content ("Project:")),
			(position => (  2, 26), size => 3, content => et_text.to_content ("Module:")),
			(position => (  2, 21), size => 3, content => et_text.to_content ("Variant:")),
			(position => (  2, 16), size => 3, content => et_text.to_content ("Customer:")),
			(position => (  2, 11), size => 3, content => et_text.to_content ("Partcode:")),
			(position => (  2,  6), size => 3, content => et_text.to_content ("Drwg. No:")),
			(position => (  2,  1), size => 3, content => et_text.to_content ("Revision:")),

			(position => (120, 16), size => 3, content => et_text.to_content ("date")),
			(position => (152, 16), size => 3, content => et_text.to_content ("name")),
			(position => ( 90, 11), size => 3, content => et_text.to_content ("edited:")),
			(position => ( 90,  6), size => 3, content => et_text.to_content ("checked:")),
			(position => ( 90,  1), size => 3, content => et_text.to_content ("approved")),

			(position => ( 90, 35), size => 3, content => et_text.to_content ("SHEET DESCRIPTION:")),
			(position => (192, 21), size => 3, content => et_text.to_content ("SHEET")),
			(position => ( 90, 21), size => 3, content => et_text.to_content ("CAT:"))
			);

		texts_pcb : constant type_texts (1 .. 13) := (
			(position => (  2, 36), size => 3, content => et_text.to_content ("Company:")),
			(position => (  2, 31), size => 3, content => et_text.to_content ("Project:")),
			(position => (  2, 26), size => 3, content => et_text.to_content ("Module:")),
			(position => (  2, 21), size => 3, content => et_text.to_content ("Variant:")),
			(position => (  2, 16), size => 3, content => et_text.to_content ("Customer:")),
			(position => (  2, 11), size => 3, content => et_text.to_content ("Partcode:")),
			(position => (  2,  6), size => 3, content => et_text.to_content ("Drwg. No:")),
			(position => (  2,  1), size => 3, content => et_text.to_content ("Revision:")),

			(position => (120, 16), size => 3, content => et_text.to_content ("date")),
			(position => (152, 16), size => 3, content => et_text.to_content ("name")),
			(position => ( 90, 11), size => 3, content => et_text.to_content ("edited:")),
			(position => ( 90,  6), size => 3, content => et_text.to_content ("checked:")),
			(position => ( 90,  1), size => 3, content => et_text.to_content ("approved"))
			);

		-- Collects the texts of the given array and returns them as a list:
		function make_texts (texts : in type_texts) return pac_texts.list is
			use pac_texts;
			result : pac_texts.list;
		begin
			for i in texts'first .. texts'last loop
				result.append (texts (i));
			end loop;
			return result;
		end make_texts;
		
	begin -- apply_defaults
		case frame.domain is
			when SCHEMATIC =>
				-- type_title_bock (basic stuff):
				frame.title_block_schematic.position										:= ( 55,  6);
				frame.title_block_schematic.lines := make_lines (lines_sch);
				frame.title_block_schematic.placeholders.project_name.position 				:= ( 30, 31);
				frame.title_block_schematic.placeholders.module_file_name.position 			:= ( 30, 26);
				frame.title_block_schematic.placeholders.active_assembly_variant.position	:= ( 30, 21);
				frame.title_block_schematic.texts := make_texts (texts_sch);

				frame.title_block_schematic.additional_placeholders.company.position 		:= ( 30, 36);
				frame.title_block_schematic.additional_placeholders.customer.position 		:= ( 30, 16);
				frame.title_block_schematic.additional_placeholders.partcode.position 		:= ( 30, 11);
				frame.title_block_schematic.additional_placeholders.drawing_number.position	:= ( 30,  6);
				frame.title_block_schematic.additional_placeholders.revision.position 		:= ( 30,  1);

				frame.title_block_schematic.additional_placeholders.drawn_by.position 		:= (152, 11);
				frame.title_block_schematic.additional_placeholders.checked_by.position 	:= (152,  6);
				frame.title_block_schematic.additional_placeholders.approved_by.position 	:= (152,  1);

				frame.title_block_schematic.additional_placeholders.drawn_date.position 	:= (120, 11);
				frame.title_block_schematic.additional_placeholders.checked_date.position 	:= (120,  6);
				frame.title_block_schematic.additional_placeholders.approved_date.position 	:= (120,  1);
			
				frame.title_block_schematic.additional_placeholders.sheet_number.position 	:= (210, 21);
				frame.title_block_schematic.additional_placeholders.description.position 	:= ( 90, 30);
				frame.title_block_schematic.additional_placeholders.category.position 		:= (105, 21);

			when PCB =>
				-- type_title_bock (basic stuff):
				frame.title_block_pcb.position												:= ( 55,  6);
				frame.title_block_pcb.lines := make_lines (lines_pcb);
				frame.title_block_pcb.placeholders.project_name.position					:= ( 30, 31);
				frame.title_block_pcb.placeholders.module_file_name.position				:= ( 30, 26);
				frame.title_block_pcb.placeholders.active_assembly_variant.position			:= ( 30, 21);
				frame.title_block_pcb.texts := make_texts (texts_pcb);

				frame.title_block_pcb.additional_placeholders.company.position 				:= ( 30, 36);
				frame.title_block_pcb.additional_placeholders.customer.position 			:= ( 30, 16);
				frame.title_block_pcb.additional_placeholders.partcode.position 			:= ( 30, 11);
				frame.title_block_pcb.additional_placeholders.drawing_number.position		:= ( 30,  6);
				frame.title_block_pcb.additional_placeholders.revision.position 			:= ( 30,  1);

				frame.title_block_pcb.additional_placeholders.drawn_by.position 			:= (152, 11);
				frame.title_block_pcb.additional_placeholders.checked_by.position 			:= (152,  6);
				frame.title_block_pcb.additional_placeholders.approved_by.position 			:= (152,  1);

				frame.title_block_pcb.additional_placeholders.drawn_date.position 			:= (120, 11);
				frame.title_block_pcb.additional_placeholders.checked_date.position 		:= (120,  6);
				frame.title_block_pcb.additional_placeholders.approved_date.position 		:= (120,  1);

				frame.title_block_pcb.additional_placeholders.face.position 				:= (120, 36);
				frame.title_block_pcb.additional_placeholders.signal_layer.position 		:= (120, 31);

				-- cam markers
				frame.title_block_pcb.cam_markers.face.position 							:= ( 90, 36);
				frame.title_block_pcb.cam_markers.signal_layer.position 					:= ( 90, 31);
				frame.title_block_pcb.cam_markers.silk_screen.position 						:= (120, 26);
				frame.title_block_pcb.cam_markers.assy_doc.position 						:= (150, 26);
				frame.title_block_pcb.cam_markers.stop_mask.position 						:= (175, 26);
				frame.title_block_pcb.cam_markers.stencil.position 							:= (190, 26);
				frame.title_block_pcb.cam_markers.pcb_outline.position 						:= (120, 21);
				frame.title_block_pcb.cam_markers.plated_millings.position					:= (145, 21);
				frame.title_block_pcb.cam_markers.keepout.position 							:= (185, 21);
				frame.title_block_pcb.cam_markers.route_restrict.position 					:= (120, 16);
				frame.title_block_pcb.cam_markers.via_restrict.position 					:= (165, 16);
		end case;
	end apply_defaults;
	

	function to_string (cat : in type_schematic_sheet_category) return string is begin
		return type_schematic_sheet_category'image (cat);
	end;

	function to_category (cat : in string) return type_schematic_sheet_category is begin
		return type_schematic_sheet_category'value (cat);
	end;
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
