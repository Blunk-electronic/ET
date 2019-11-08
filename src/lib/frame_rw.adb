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
with ada.characters.handling;
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

	function to_position (line : in et_string_processing.type_fields_of_line)
	-- Converts the fields like "x 220 y 239" of a line to a type_position.
	-- Starts processing from field 2. Ignores field 1 and fields after field 5.
		return type_position is
		use et_geometry; -- for keywords only
		position : type_position;

		procedure error (p : positive) is begin invalid_keyword (f (line, p)); end;
	
	begin
		for place in 2 .. 5 loop
			case place is
				when 2 => if f (line, place) /= keyword_x then error (place); end if; -- expect an x
				when 3 => position.x := to_distance (f (line, place)); -- expect x value
				when 4 => if f (line, place) /= keyword_y then error (place); end if; -- expect an y
				when 5 => position.y := to_distance (f (line, place)); -- expect y value
			end case;
		end loop;

		-- CS exception handler in case x or y value is invalid
		return position;
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

				-- CS in the future, if a line has a width, write it here
				section_mark (section_line, FOOTER);
			end;
				
		begin -- write lines
			section_mark (section_lines, HEADER);
			iterate (lines, write'access);
			section_mark (section_lines, FOOTER);
		end;

		procedure write_placeholder (ph : in type_placeholder) is begin
			-- position
			write (keyword => keyword_position, parameters	=> to_string (ph.position)); -- position x 220 y 40

			-- size
			write (keyword => et_text.keyword_size, parameters => to_string (ph.size)); -- size 20
		end;

		procedure write_text (text : in type_text) is begin
			section_mark (section_text, HEADER);
			
			-- position
			write (keyword => keyword_position, parameters	=> to_string (text.position)); -- position x 220 y 40

			-- size
			write (keyword => et_text.keyword_size, parameters => to_string (text.size)); -- size 20

			-- content
			write (keyword => et_text.keyword_content, wrap => true,
				   parameters => et_text.to_string (text.content)); -- content "motor driver"

			section_mark (section_text, FOOTER);
		end;

		procedure write_cam_marker (cm : in type_cam_marker) is begin
			-- position
			write (keyword => keyword_position, parameters	=> to_string (cm.position)); -- position x 220 y 40

			-- size
			write (keyword => et_text.keyword_size, parameters => to_string (cm.size)); -- size 20

			-- content
			write (keyword => et_text.keyword_content, wrap => true,
				   parameters => et_text.to_string (cm.content));
		end;
		
		procedure write_texts (texts : in pac_texts.list) is
			use pac_texts;

			procedure write (cursor : in pac_texts.cursor) is begin
				write_text (element (cursor));
			end;
			
		begin -- write_texts
			iterate (texts, write'access);
		end write_texts;

		procedure write_placeholders_common (ph : in type_placeholders_common) is begin
			section_mark (section_project_name, HEADER);
			write_placeholder (ph.project_name);
			section_mark (section_project_name, FOOTER);

			section_mark (section_module_file_name, HEADER);			
			write_placeholder (ph.module_file_name);
			section_mark (section_module_file_name, FOOTER);

			section_mark (section_active_assembly_variant, HEADER);
			write_placeholder (ph.active_assembly_variant);
			section_mark (section_active_assembly_variant, FOOTER);			
		end write_placeholders_common;

		procedure write_placeholders_basic (ph : in type_placeholders_basic) is begin
			section_mark (section_company, HEADER);
			write_placeholder (ph.company);
			section_mark (section_company, FOOTER);

			section_mark (section_customer, HEADER);			
			write_placeholder (ph.customer);
			section_mark (section_customer, FOOTER);			

			section_mark (section_partcode, HEADER);			
			write_placeholder (ph.partcode);
			section_mark (section_partcode, FOOTER);			

			section_mark (section_drawing_number, HEADER);			
			write_placeholder (ph.drawing_number);
			section_mark (section_drawing_number, FOOTER);

			section_mark (section_revision, HEADER);			
			write_placeholder (ph.revision);
			section_mark (section_revision, FOOTER);						

			section_mark (section_drawn_by, HEADER);			
			write_placeholder (ph.drawn_by);
			section_mark (section_drawn_by, FOOTER);			

			section_mark (section_checked_by, HEADER);
			write_placeholder (ph.checked_by);
			section_mark (section_checked_by, FOOTER);

			section_mark (section_approved_by, HEADER);
			write_placeholder (ph.approved_by);
			section_mark (section_approved_by, FOOTER);

			section_mark (section_drawn_date, HEADER);
			write_placeholder (ph.drawn_date);
			section_mark (section_drawn_date, FOOTER);

			section_mark (section_checked_date, HEADER);
			write_placeholder (ph.checked_date);
			section_mark (section_checked_date, FOOTER);

			section_mark (section_approved_date, HEADER);			
			write_placeholder (ph.approved_date);
			section_mark (section_approved_date, FOOTER);			
		end;
		
		procedure write_placeholders_schematic (ph : in type_placeholders_schematic) is begin
			write_placeholders_basic (type_placeholders_basic (ph));

			section_mark (section_sheet_number, HEADER);
			write_placeholder (ph.sheet_number);
			section_mark (section_sheet_number, FOOTER);

			section_mark (section_sheet_description, HEADER);			
			write_placeholder (ph.description);
			section_mark (section_sheet_description, FOOTER);

			section_mark (section_sheet_category, HEADER);						
			write_placeholder (ph.category);
			section_mark (section_sheet_category, FOOTER);			
		end;

		procedure write_placeholders_pcb (ph : in type_placeholders_pcb) is begin
			write_placeholders_basic (type_placeholders_basic (ph));

			section_mark (section_face, HEADER);
			write_placeholder (ph.face);
			section_mark (section_face, FOOTER);

			section_mark (section_signal_layer, HEADER);
			write_placeholder (ph.signal_layer);
			section_mark (section_signal_layer, FOOTER);
			
		end;
		
-- 		procedure write_text_pcb (texts : in type_texts_pcb) is begin
-- 			write_text (texts.face);
-- 			write_text (texts.top);
-- 			write_text (texts.bottom);
-- 			write_text (texts.silk_screen);
-- 			write_text (texts.assy_doc);
-- 			write_text (texts.keepout);			
-- 			write_text (texts.plated_millings);
-- 			write_text (texts.pcb_outline);
-- 			write_text (texts.route_restrict);
-- 			write_text (texts.via_restrict);
-- 			write_text (texts.signal_layer);
-- 		end;

		procedure write_cam_markers (cms : in type_cam_markers) is begin
			section_mark (section_cam_markers, HEADER);
			
			section_mark (section_face, HEADER);
			write_cam_marker (cms.face);
			section_mark (section_face, FOOTER);

			section_mark (section_silk_screen, HEADER);
			write_cam_marker (cms.silk_screen);
			section_mark (section_silk_screen, FOOTER);

			section_mark (section_assy_doc, HEADER);
			write_cam_marker (cms.assy_doc);
			section_mark (section_assy_doc, FOOTER);

			section_mark (section_keepout, HEADER);
			write_cam_marker (cms.keepout);
			section_mark (section_keepout, FOOTER);

			section_mark (section_plated_millings, HEADER);
			write_cam_marker (cms.plated_millings);
			section_mark (section_plated_millings, FOOTER);

			section_mark (section_pcb_outline, HEADER);
			write_cam_marker (cms.pcb_outline);
			section_mark (section_pcb_outline, FOOTER);

			section_mark (section_route_restrict, HEADER);
			write_cam_marker (cms.route_restrict);
			section_mark (section_route_restrict, FOOTER);

			section_mark (section_via_restrict, HEADER);
			write_cam_marker (cms.via_restrict);
			section_mark (section_via_restrict, FOOTER);

			section_mark (section_signal_layer, HEADER);
			write_cam_marker (cms.signal_layer);
			section_mark (section_signal_layer, FOOTER);

			section_mark (section_cam_markers, FOOTER);			
		end;

		
		procedure write_title_block (block : in type_title_block'class) is 
			use ada.tags;
			pp : type_placeholders_pcb;
			ps : type_placeholders_schematic;
		begin
			write (keyword => keyword_position, parameters => to_string (block.position)); -- position x 180 x 10

			-- write lines. they are basic elements of a title block:
			write_lines (block.lines);


			
			-- write texts (with content). they are basic elements of a title block
			section_mark (section_texts, HEADER);
			write_texts (block.texts);

-- 			-- if the given title block belongs to a layout frame, write additional texts (with content):
-- 			if block'tag = type_title_block_pcb'tag then
-- 				tp := type_title_block_pcb (block).additional_texts;
-- 				write_text_pcb (tp);
-- 			end if;

			section_mark (section_texts, FOOTER);
			
			
			section_mark (section_placeholders, HEADER);
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

			section_mark (section_placeholders, FOOTER);

			-- if the given title block belongs to a layout frame, write cam markers:
			if block'tag = type_title_block_pcb'tag then
				write_cam_markers (type_title_block_pcb (block).cam_markers);
			end if;
			
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

		frame : type_frame (domain); -- to be returned

		file_handle : ada.text_io.file_type;

		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the frame.
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 4; -- incl. section init

		package stack is new general_rw.stack_lifo (
			item	=> type_section,
			max 	=> max_section_depth);

		use ada.characters.handling;
		
		function to_string (section : in type_section) return string is
		-- Converts a section like SEC_PROJECT_NAME to a string "project_name".
			len : positive := type_section'image (section)'length;
		begin
			return to_lower (type_section'image (section) (5..len));
		end to_string;

		procedure invalid_domain is begin
			log (ERROR, text => "invalid domain ", console => true);
			-- CS improve message
			raise constraint_error;
		end;

		procedure read_general_stuff is
			kw : string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_domain then -- domain schematic/pcb
				expect_field_count (line, 2);

				-- The given domain must match the domain specified in the frame:
				if to_domain (f (line, 2)) /= domain then
					invalid_domain;
				end if;

			elsif kw = keyword_paper_size then -- paper_size A4
				expect_field_count (line, 2);
				frame.paper := to_paper_size (f (line, 2));

			elsif kw = keyword_orientation then -- orientation landscape/portrait
				expect_field_count (line, 2);
				frame.orientation := to_orientation (f (line, 2));

			elsif kw = keyword_border_width then -- border_width 8
				expect_field_count (line, 2);
				frame.border_width := to_distance (f (line, 2));

			elsif kw = keyword_size then -- size x 280 y 200
				expect_field_count (line, 5);
				frame.size.x := to_distance (f (line, 3));
				frame.size.y := to_distance (f (line, 5));
				-- CS check position of x and y character

			elsif kw = keyword_sectors then -- sectors rows 7 columns 10
				expect_field_count (line, 5);
				frame.sectors.rows := to_rows (f (line, 3));
				frame.sectors.columns := to_columns (f (line, 5));
				-- CS check position of keywords row and column

			else
				invalid_keyword (kw);
			end if;
		end;

		-- TEMPORARILY VARIABLES AND CONTAINERS
		tb_position 	: type_position;
		tb_line 		: type_line;
		tb_lines		: pac_lines.list;
		tb_text			: type_text;
		tb_texts		: pac_texts.list;
		tb_placeholder	: type_placeholder;
		tb_placeholders_common	: type_placeholders_common;
		tb_placeholders_basic	: type_placeholders_basic;
		tb_sheet_number			: type_placeholder; -- for schematic only
		tb_sheet_description 	: type_placeholder; -- for schematic only
		tb_sheet_category		: type_placeholder; -- for schematic only
		tb_face					: type_placeholder; -- for pcb only
		tb_signal_layer			: type_placeholder; -- for pcb only
		tb_cam_marker			: type_cam_marker;
		
		procedure read_title_block_position is
			use et_geometry; -- for keywords only
			kw : constant string := f (line, 1);
		begin
			if kw = keyword_position then -- position x 100 y 200
				expect_field_count (line, 5);
				tb_position := to_position (line);
			end if;
		end;
		
		procedure read_line_properties is
			use et_geometry; -- for keywords only
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_start then -- start x 220 y 239
				expect_field_count (line, 5);
				tb_line.start_point := to_position (line);

			elsif kw = keyword_end then -- end x 250 y 239
				expect_field_count (line, 5);
				tb_line.end_point := to_position (line);
			else
				invalid_keyword (kw);
			end if;
		end;

		procedure read_text_properties is
			use et_geometry; -- for keywords only
			use et_text; -- for keywords only
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 220 y 239
				expect_field_count (line, 5);
				tb_text.position := to_position (line);

			elsif kw = keyword_size then -- size 12
				expect_field_count (line, 2);
				tb_text.size := to_distance (f (line, 2));

			elsif kw = keyword_content then -- content "some text"
				expect_field_count (line, 2);
				tb_text.content := to_content (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end;

		procedure read_cam_marker_properties is
			use et_geometry; -- for keywords only
			use et_text; -- for keywords only
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 220 y 239
				expect_field_count (line, 5);
				tb_cam_marker.position := to_position (line);

			elsif kw = keyword_size then -- size 12
				expect_field_count (line, 2);
				tb_cam_marker.size := to_distance (f (line, 2));

			elsif kw = keyword_content then -- content "some text"
				expect_field_count (line, 2);
				tb_cam_marker.content := to_content (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end;
		
		procedure read_placeholder_properties is
			use et_geometry; -- for keywords only
			use et_text; -- for keywords only
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 220 y 239
				expect_field_count (line, 5);
				tb_placeholder.position := to_position (line);

			elsif kw = keyword_size then -- size 12
				expect_field_count (line, 2);
				tb_placeholder.size := to_distance (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end;

		
		procedure reset_placeholder is begin tb_placeholder := (others => <>); end;

		procedure reset_cam_marker is begin tb_cam_marker := (others => <>); end;
		
		procedure assemble_title_block is 
			use pac_lines;
			use pac_texts;
		begin
								
			case domain is
				when SCHEMATIC => 
					frame.title_block_schematic.position := tb_position;
					frame.title_block_schematic.lines := tb_lines;
					frame.title_block_schematic.texts := tb_texts;
					frame.title_block_schematic.placeholders := tb_placeholders_common;
					frame.title_block_schematic.additional_placeholders := (
						tb_placeholders_basic with 
							sheet_number 	=> tb_sheet_number,
							description		=> tb_sheet_description,
							category		=> tb_sheet_category);
					
				when PCB =>
					frame.title_block_pcb.position := tb_position;
					frame.title_block_pcb.lines := tb_lines;
					frame.title_block_pcb.texts := tb_texts;
					frame.title_block_pcb.placeholders := tb_placeholders_common;
					frame.title_block_pcb.additional_placeholders := (
						tb_placeholders_basic with 
							face			=> tb_face,
							signal_layer	=> tb_signal_layer);
			end case;

			-- clean up (even if there is no further title block)
			tb_position := (others => <>);
			clear (tb_lines);
			clear (tb_texts);
			tb_placeholders_common := (others => <>);
		end;
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
				use pac_lines;
				use pac_texts;
				
			begin -- execute_section
				case stack.current is

					when SEC_TITLE_BLOCK => 
						case stack.parent is
							when SEC_INIT => assemble_title_block;

							when others => invalid_section;
						end case;

					when SEC_LINES | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_CAM_MARKERS =>
						case stack.parent is
							when SEC_TITLE_BLOCK => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_LINES =>
								-- append the title block line to the collection of lines
								append (tb_lines, tb_line);

								-- clean up for next title block line
								tb_line := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								-- append the title block text to the collection of texts
								append (tb_texts, tb_text);

								-- clean up for next title block text
								tb_text := (others => <>);
								
							when others => invalid_section;
						end case;
						
					when SEC_PROJECT_NAME =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_common.project_name := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_MODULE_FILE_NAME =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_common.module_file_name := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_ACTIVE_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_common.active_assembly_variant := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_COMPANY =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.company := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_CUSTOMER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.customer := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_PARTCODE =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.partcode := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_DRAWING_NUMBER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.drawing_number := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_REVISION =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.revision := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_DRAWN_BY =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.drawn_by := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_DRAWN_DATE =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.drawn_date := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;
						
					when SEC_CHECKED_BY =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.checked_by := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_CHECKED_DATE =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.checked_date := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_APPROVED_BY =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.approved_by := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_APPROVED_DATE =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								tb_placeholders_basic.approved_date := tb_placeholder;
								reset_placeholder;
							when others => invalid_section;
						end case;

					when SEC_SHEET_NUMBER => -- NOTE: this placeholder exists in schematic only !
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case domain is
									when SCHEMATIC => tb_sheet_number := tb_placeholder;
									when others => invalid_section;
								end case;
							when others => invalid_section;
						end case;
						
					when SEC_SHEET_DESCRIPTION => -- NOTE: this placeholder exists in schematic only !
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case domain is
									when SCHEMATIC => tb_sheet_description := tb_placeholder;
									when others => invalid_section;
								end case;
							when others => invalid_section;
						end case;

					when SEC_SHEET_CATEGORY => -- NOTE: this placeholder exists in schematic only !
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case domain is
									when SCHEMATIC => tb_sheet_category := tb_placeholder;
									when others => invalid_section;
								end case;
							when others => invalid_section;
						end case;

					when SEC_FACE => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_PLACEHOLDERS => tb_face := tb_placeholder;
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.face := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_SIGNAL_LAYER => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_PLACEHOLDERS => tb_signal_layer := tb_placeholder;
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.signal_layer := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_SILK_SCREEN => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.silk_screen := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_ASSY_DOC => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.assy_doc := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_KEEPOUT => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS =>
										frame.title_block_pcb.cam_markers.keepout := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_PLATED_MILLINGS => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.plated_millings := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_PCB_OUTLINE => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.pcb_outline := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;
						
					when SEC_ROUTE_RESTRICT => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => 
										frame.title_block_pcb.cam_markers.route_restrict := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_VIA_RESTRICT => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS =>
										frame.title_block_pcb.cam_markers.via_restrict := tb_cam_marker;
										reset_cam_marker;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;
						
					when SEC_INIT => null; -- CS: should never happen

				end case;

			end execute_section;

			function set (
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
				section_keyword	: in string;
				section			: in type_section) -- SEC_PROJECT_NAME
				return boolean is 
			begin
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						execute_section;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
						end if;
						return true;

					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;

				else -- neither a section header nor footer
					return false;
				end if;
			end set;

		begin -- process_line
			if set (section_active_assembly_variant, SEC_ACTIVE_ASSEMBLY_VARIANT) then null;			
			elsif set (section_approved_by, SEC_APPROVED_BY) then null;								
			elsif set (section_approved_date, SEC_APPROVED_DATE) then null;
			elsif set (section_assy_doc, SEC_ASSY_DOC) then null;
			elsif set (section_cam_markers, SEC_CAM_MARKERS) then null;
			elsif set (section_checked_by, SEC_CHECKED_BY) then null;
			elsif set (section_checked_date, SEC_CHECKED_DATE) then null;
			elsif set (section_company, SEC_COMPANY) then null;
			elsif set (section_customer, SEC_CUSTOMER) then null;
			elsif set (section_drawing_number, SEC_DRAWING_NUMBER) then null;
			elsif set (section_drawn_by, SEC_DRAWN_BY) then null;
			elsif set (section_drawn_date, SEC_DRAWN_DATE) then null;
			elsif set (section_face, SEC_FACE) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;
			elsif set (section_line, SEC_LINE) then null;
			elsif set (section_lines, SEC_LINES) then null;
			elsif set (section_module_file_name, SEC_MODULE_FILE_NAME) then null;
			elsif set (section_partcode, SEC_PARTCODE) then null;
			elsif set (section_pcb_outline, SEC_PCB_OUTLINE) then null;
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;
			elsif set (section_plated_millings, SEC_PLATED_MILLINGS) then null;			
			elsif set (section_project_name, SEC_PROJECT_NAME) then null;
			elsif set (section_revision, SEC_REVISION) then null;
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;
			elsif set (section_sheet_category, SEC_SHEET_CATEGORY) then null;
			elsif set (section_sheet_description, SEC_SHEET_DESCRIPTION) then null;			
			elsif set (section_sheet_number, SEC_SHEET_NUMBER) then null;
			elsif set (section_signal_layer, SEC_SIGNAL_LAYER) then null;
			elsif set (section_silk_screen, SEC_SILK_SCREEN) then null;
			elsif set (section_text, SEC_TEXT) then null;	
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_title_block, SEC_TITLE_BLOCK) then null;
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "frame line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						read_general_stuff;

					when SEC_TITLE_BLOCK => 
						case stack.parent is
							when SEC_INIT => read_title_block_position;
							when others => invalid_section;
						end case;

					when SEC_LINES | SEC_TEXTS | SEC_PLACEHOLDERS | SEC_CAM_MARKERS =>
						case stack.parent is
							when SEC_TITLE_BLOCK => null;
							when others => invalid_section;
						end case;

					when SEC_LINE =>
						case stack.parent is
							when SEC_LINES => read_line_properties;
							when others => invalid_section;
						end case;
						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS => read_text_properties;
							when others => invalid_section;
						end case;
						
					when SEC_PROJECT_NAME | SEC_MODULE_FILE_NAME | SEC_ACTIVE_ASSEMBLY_VARIANT |
						SEC_COMPANY | SEC_CUSTOMER | SEC_PARTCODE | SEC_DRAWING_NUMBER | SEC_REVISION |
						SEC_DRAWN_BY | SEC_DRAWN_DATE | SEC_CHECKED_BY | SEC_CHECKED_DATE | 
						SEC_APPROVED_BY | SEC_APPROVED_DATE =>
						case stack.parent is
							when SEC_PLACEHOLDERS => read_placeholder_properties;
							when others => invalid_section;
						end case;

					when SEC_SHEET_NUMBER | SEC_SHEET_DESCRIPTION | SEC_SHEET_CATEGORY => 
						-- NOTE: these placeholders exists in schematic only !
						case stack.parent is
							when SEC_PLACEHOLDERS => 
								case domain is
									when SCHEMATIC => read_placeholder_properties;
									when others => invalid_section;
								end case;
							when others => invalid_section;
						end case;
						
					when SEC_FACE => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_PLACEHOLDERS => read_placeholder_properties;
									when SEC_CAM_MARKERS => read_cam_marker_properties;
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_SIGNAL_LAYER => -- NOTE: this section exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_PLACEHOLDERS => read_placeholder_properties;
									when SEC_CAM_MARKERS => read_cam_marker_properties;
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_SILK_SCREEN | SEC_ASSY_DOC | SEC_KEEPOUT | SEC_PLATED_MILLINGS |
						SEC_PCB_OUTLINE | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => -- NOTE: these sections exists in pcb only !
						case domain is
							when PCB =>
								
								case stack.parent is
									when SEC_CAM_MARKERS => read_cam_marker_properties;
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
				end case;
			end if;

			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;

		
		previous_input : ada.text_io.file_type renames current_input;
		
	begin -- read_frame
		log (text => "reading frame " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (domain) & " ...", level => log_threshold);

		open (
			file => file_handle,
			mode => in_file, 
			name => expand (to_string (file_name)));

		set_input (file_handle);
		
		-- Init section stack.
		stack.init;
		stack.push (SEC_INIT);

		-- read the file line by line
		while not end_of_file loop
			line := et_string_processing.read_line (
				line 			=> get_line,
				number			=> ada.text_io.line (current_input),
				comment_mark 	=> comment_mark,
				delimiter_wrap	=> true, -- strings are enclosed in quotations
				ifs 			=> space); -- fields are separated by space

			-- we are interested in lines that contain something. emtpy lines are skipped:
			if field_count (line) > 0 then
				process_line;
			end if;
		end loop;

		-- As a safety measure the top section must be reached finally.
		if stack.depth > 1 then 
			log (WARNING, write_section_stack_not_empty);
		end if;

		set_input (previous_input);
		close (file_handle);


		
		log_indentation_down;

		return frame;

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;
		
	end read_frame;
	
end frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
