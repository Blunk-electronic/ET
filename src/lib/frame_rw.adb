------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              FRAME_RW                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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


with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.tags;
with ada.exceptions;

with et_geometry;
with et_coordinates;
with et_general;				use et_general;
with et_text;
with et_frames;					use et_frames;
with et_string_processing;		use et_string_processing;
with general_rw;				use general_rw;

package body frame_rw is

	function to_string (position : in type_position) return string is
	-- returns something like "x 120 y 12"
		use et_geometry;
	begin
		return 
			keyword_x & space & to_string (position.x) & space &
			keyword_y & space & to_string (position.y);
	end;
	
	procedure write (
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_geometry;  -- for keywords only		
		
		file_handle : ada.text_io.file_type;

		procedure write_lines (lines : in pac_lines.list) is
			use pac_lines;

			procedure write (cursor : in pac_lines.cursor) is begin
				section_mark (section_line, HEADER);

				-- start point
				write (
					keyword		=> keyword_start, 
					parameters	=> to_string (element (cursor).start_point)); -- start x 180 x 10

				-- end point
				write (
					keyword		=> keyword_end, 
					parameters	=> to_string (element (cursor).end_point)); -- end x 180 x 10
				
				section_mark (section_line, FOOTER);
			end;
				
		begin -- write lines
			section_mark (section_lines, HEADER);
			iterate (lines, write'access);
			section_mark (section_lines, FOOTER);
		end;

		procedure write_texts (texts : in pac_texts.list) is
			use pac_texts;

			procedure write (cursor : in pac_texts.cursor) is begin
				section_mark (section_text, HEADER);

				-- position
				write (
					keyword		=> keyword_position,
					parameters	=> to_string (element (cursor).position)); -- position x 220 y 40

				-- size
				write (
					keyword		=> et_text.keyword_size,
					parameters	=> to_string (element (cursor).size)); -- size 20

				-- content
				write (
					keyword		=> et_text.keyword_content,
					wrap		=> true,
					parameters	=> et_text.to_string (element (cursor).content)); -- content "motor driver"
				
				section_mark (section_text, FOOTER);
			end;
			
		begin -- write_texts
			section_mark (section_texts, HEADER);
			iterate (texts, write'access);
			section_mark (section_texts, FOOTER);
		end write_texts;

		procedure write_placeholders_common (phs : in type_placeholders_common) is begin
			write (keyword => keyword_project_name, parameters => to_string (phs.project_name.position));
			write (keyword => keyword_module_file_name, parameters => to_string (phs.module_file_name.position));
			write (keyword => keyword_active_assembly_variant, parameters => to_string (phs.active_assembly_variant.position));
		end write_placeholders_common;

		procedure write_placeholders_basic (phb : in type_placeholders_basic) is begin
			write (keyword => keyword_company, parameters => to_string (phb.company.position));
			write (keyword => keyword_customer, parameters => to_string (phb.customer.position));
			write (keyword => keyword_partcode, parameters => to_string (phb.partcode.position));
			write (keyword => keyword_drawing_number, parameters => to_string (phb.drawing_number.position));
			write (keyword => keyword_revision, parameters => to_string (phb.revision.position));
			write (keyword => keyword_drawn_by, parameters => to_string (phb.drawn_by.position));
			write (keyword => keyword_checked_by, parameters => to_string (phb.checked_by.position));
			write (keyword => keyword_approved_by, parameters => to_string (phb.approved_by.position));
			write (keyword => keyword_drawn_date, parameters => to_string (phb.drawn_date.position));
			write (keyword => keyword_checked_date, parameters => to_string (phb.checked_date.position));
			write (keyword => keyword_approved_date, parameters => to_string (phb.approved_date.position));
		end;
		
		procedure write_placeholders_schematic (phs : in type_placeholders_schematic) is begin
			write_placeholders_basic (type_placeholders_basic (phs));
			write (keyword => keyword_sheet_number, parameters => to_string (phs.sheet_number.position));
			write (keyword => keyword_sheet_description, parameters => to_string (phs.description.position));
			write (keyword => keyword_sheet_category, parameters => to_string (phs.category.position));
		end;

		procedure write_placeholders_pcb (phs : in type_placeholders_pcb) is begin
			write_placeholders_basic (type_placeholders_basic (phs));
			write (keyword => keyword_silk_screen, parameters => to_string (phs.silk_screen.position));
			write (keyword => keyword_assy_doc, parameters => to_string (phs.assy_doc.position));
			write (keyword => keyword_keepout, parameters => to_string (phs.keepout.position));			
			write (keyword => keyword_plated_millings, parameters => to_string (phs.plated_millings.position));
			write (keyword => keyword_pcb_outline, parameters => to_string (phs.pcb_outline.position));
			write (keyword => keyword_route_restrict, parameters => to_string (phs.route_restrict.position));
			write (keyword => keyword_via_restrict, parameters => to_string (phs.via_restrict.position));
			write (keyword => keyword_signal_layer, parameters => to_string (phs.signal_layer.position));
		end;
		
		procedure write_title_block (block : in type_title_block'class) is 
			use ada.tags;
			pp : type_placeholders_pcb;
			ps : type_placeholders_schematic;
		begin
			write (keyword => keyword_position, parameters => to_string (block.position)); -- position x 180 x 10

			write_lines (block.lines);
			write_texts (block.texts);

			section_mark (section_placeholder_positions, HEADER);
			write_placeholders_common (block.placeholders);

			if block'tag = type_title_block_schematic'tag then
				ps := type_title_block_schematic (block).additional_placeholders;
				write_placeholders_schematic (ps);
			elsif block'tag = type_title_block_pcb'tag then
				pp := type_title_block_pcb (block).additional_placeholders;
				write_placeholders_pcb (pp);
			else
				null; -- CS
			end if;

			section_mark (section_placeholder_positions, FOOTER);
			
		end write_title_block;
		
	begin -- write
		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);

		-- write a nice header
		put_line (comment_mark & space & et_general.system_name & space & "drawing frame template");
		put_line (comment_mark & space & date);
		put_line (comment_mark & space & row_separator_double);
		new_line;



		
		write (keyword => keyword_domain, parameters => to_string (frame.domain));
		write (keyword => keyword_paper_size, parameters => to_string (frame.paper));
		write (keyword => keyword_orientation, parameters => to_string (frame.orientation));
		write (keyword => keyword_border_width, parameters => to_string (frame.border_width));
		write (keyword => keyword_size, parameters => 
			keyword_x & space & to_string (frame.size.x) & space &
			keyword_y & space & to_string (frame.size.y)); -- size x 240 x 200

		write (keyword => keyword_sectors, parameters => 
			keyword_rows & space & to_string (frame.sectors.rows) & space &
			keyword_columns & space & to_string (frame.sectors.columns)); -- sectors rows 5 columns 10



		
		-- title block
		section_mark (section_title_block, HEADER);
		
		-- write general things of title block (standard placeholders, texts, lines):
		case frame.domain is 
			when SCHEMATIC =>
				write_title_block (frame.title_block_schematic);

			when PCB =>
				write_title_block (frame.title_block_pcb);
		end case;

		
		section_mark (section_title_block, FOOTER);		


		
		-- write footer
		new_line;		
		put_line (comment_mark & space & row_separator_double);
		put_line (comment_mark & space & "drawing frame template file end");
		new_line;
		
		reset_tab_depth;		


		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;

	end write;
	
	procedure create_frame (
	-- Creates and saves a frame in given file_name.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;							   
		log_threshold	: in et_string_processing.type_log_level) is

		frame : type_frame (domain);
	begin
		log (text => "creating frame " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (domain) & " ...", level => log_threshold);

		write (frame, file_name, log_threshold + 1);
		
		log_indentation_down;
	end create_frame;
	
	procedure save_frame (
	-- Saves the given frame in file_name.
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	begin
		log (text => "saving frame as " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (frame.domain) & " ...", level => log_threshold);

		write (frame, file_name, log_threshold + 1);
		
		log_indentation_down;
	end save_frame;

	function read_frame (
	-- Reads a frame from given file_name and returns a parameterized type_frame.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;
		log_threshold	: in et_string_processing.type_log_level)
		return type_frame is

		frame : type_frame (SCHEMATIC);
	begin
		log (text => "reading frame " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (domain) & " ...", level => log_threshold);

		log_indentation_down;

		return frame;
	end read_frame;
	
end frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
