------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD PCB                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.directories;
with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.exceptions;

with et_general;
with et_libraries;
with et_schematic;
with et_kicad;
with et_pcb;
with et_pcb_coordinates;
with et_pcb_math;
with et_string_processing;		use et_string_processing;

with et_kicad;

package body et_kicad_pcb is

	function to_plot_output_directory (directory : in string) return type_plot_output_directory.bounded_string is
	begin
		return type_plot_output_directory.to_bounded_string (directory);
	end to_plot_output_directory;
	
	function to_string (directory : in type_plot_output_directory.bounded_string) return string is
	begin
		return type_plot_output_directory.to_string (directory);
	end to_string;

	
	function right_net_before_left (right, left : in type_netlist_net) return boolean is
	-- Returns true if the right net id comes beforr the left net id AND
	-- if the right net name differs from the left net name.
		use et_schematic.type_net_name;
	begin
		if 
			right.id > left.id 
			and
			right.name /= left.name

		then return true;
		else return false;
		
		end if;
	end right_net_before_left;

	function right_net_equals_left (right, left : in type_netlist_net) return boolean is
	-- Returns true if the right net id equals the left net id OR
	-- if the right net name equals the left net name.
		use et_schematic.type_net_name;
	begin
		if 
			right.id = left.id 
			or 
			right.name = left.name 
			
		then return true;
		else return false;
		
		end if;
	end right_net_equals_left;
	
	function to_assembly_technology (tech : in string) return et_pcb.type_assembly_technology is
		use et_pcb;
	begin
		if tech = "smd" then return SMT;
		elsif tech = "thru_hole" then return THT;
		else
			log_indentation_reset;
			log (message_error & "invalid assembly technology", console => true);
			raise constraint_error;
		end if;
	end to_assembly_technology;
			
	function to_terminal_shape_tht (shape : in string) return et_pcb.type_terminal_shape_tht is
		use et_pcb;
	begin
		if shape = "rect" then return RECTANGLE;
		elsif shape = "oval" then return LONG;
		else
			log_indentation_reset;
			log (message_error & "invalid shape for a THT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_terminal_shape_tht;

	function to_terminal_shape_smt (shape : in string) return et_pcb.type_terminal_shape_smt is
		use et_pcb;
	begin
		if shape = "rect" then return RECTANGLE;
		elsif shape = "oval" then return LONG;
		elsif shape = "circle" then return CIRCULAR;
		else
			log_indentation_reset;
			log (message_error & "invalid shape for an SMT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_terminal_shape_smt;


	function contour_milled_rectangle_of_pad (
	-- Converts the given position and dimensions of a rectangular slotted hole
	-- to a list with four lines (top, bottom, right, left).
		center		: et_pcb_coordinates.type_terminal_position; -- the terminal position (incl. angle, (z axis ignored))
		size_x		: et_pcb.type_pad_size;	-- the size in x of the hole
		size_y		: et_pcb.type_pad_size;	-- the size in y of the hole
		offset_x	: et_pcb.type_pad_drill_offset;	-- the offset of the hole from the center in x
		offset_y	: et_pcb.type_pad_drill_offset)	-- the offset of the hole from the center in y
		return et_pcb.type_pcb_contour_lines.list is

		use et_pcb;
		use et_pcb_coordinates;
	
		lines : type_pcb_contour_lines.list; -- to be returned
		line_horizontal_bottom	: type_pcb_contour_line;
		line_horizontal_top		: type_pcb_contour_line;	
		line_vertical_left 		: type_pcb_contour_line;
		line_vertical_right 	: type_pcb_contour_line;
		size_x_half : type_pad_size;
		size_y_half : type_pad_size;
	begin
		size_x_half := size_x * 0.5;
		size_y_half := size_y * 0.5;

		-- HORIZONAL LINES
		
		-- bottom
		-- start point x
		set_point (X, 
			value => offset_x + get_axis (X, type_point_3d (center)) - size_x_half,
			point => line_horizontal_bottom.start_point);

		-- start point y
		set_point (Y, 
			value => offset_y + get_axis (Y, type_point_3d (center)) - size_y_half,
			point => line_horizontal_bottom.start_point);

		-- rotate start point by angle of pad
		rotate (point => line_horizontal_bottom.start_point, angle => get_angle (center));
		
		-- end point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) + size_x_half,
			point => line_horizontal_bottom.end_point);

		-- end point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) - size_y_half,
			point => line_horizontal_bottom.end_point);

		-- rotate end point by angle of pad
		rotate (point => line_horizontal_bottom.end_point, angle => get_angle (center));


		-- top
		-- start point x
		set_point (X, 
			value => offset_x + get_axis (X, type_point_3d (center)) - size_x_half,
			point => line_horizontal_top.start_point);

		-- start point y
		set_point (Y, 
			value => offset_y + get_axis (Y, type_point_3d (center)) + size_y_half,
			point => line_horizontal_top.start_point);

		-- rotate start point by angle of pad
		rotate (point => line_horizontal_top.start_point, angle => get_angle (center));
		
		-- end point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) + size_x_half,
			point => line_horizontal_top.end_point);

		-- end point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) + size_y_half,
			point => line_horizontal_top.end_point);

		-- rotate end point by angle of pad
		rotate (point => line_horizontal_top.end_point, angle => get_angle (center));


		

		-- VERTICAL LINE

		-- left
		-- start point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) - size_x_half,
			point => line_vertical_left.start_point);

		-- start point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) - size_y_half,
			point => line_vertical_left.start_point);

		-- rotate start point by angle of pad
		rotate (point => line_vertical_left.start_point, angle => get_angle (center));
		
		-- end point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) - size_x_half,
			point => line_vertical_left.end_point);

		-- end point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) + size_y_half,
			point => line_vertical_left.end_point);

		-- rotate end point by angle of pad
		rotate (point => line_vertical_left.end_point, angle => get_angle (center));



		-- right
		-- start point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) + size_x_half,
			point => line_vertical_right.start_point);

		-- start point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) - size_y_half,
			point => line_vertical_right.start_point);

		-- rotate start point by angle of pad
		rotate (point => line_vertical_right.start_point, angle => get_angle (center));
		
		-- end point x
		set_point (X,
			value => offset_x + get_axis (X, type_point_3d (center)) + size_x_half,
			point => line_vertical_right.end_point);

		-- end point y
		set_point (Y,
			value => offset_y + get_axis (Y, type_point_3d (center)) + size_y_half,
			point => line_vertical_right.end_point);

		-- rotate end point by angle of pad
		rotate (point => line_vertical_right.end_point, angle => get_angle (center));


		
		lines.append (line_horizontal_top);
		lines.append (line_horizontal_bottom);
		lines.append (line_vertical_left);
		lines.append (line_vertical_right);		
		
		return lines;
	end contour_milled_rectangle_of_pad;

	
	function to_package_model (
	-- Builds a package model from the given lines.
		file_name		: in string; -- S_0201.kicad_mod
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level)
		return et_pcb.type_package_library is
		
		use et_pcb;
		use et_pcb.type_lines;

		-- Extract the actual package name (like S_0201) from the given file name:
		package_name : et_libraries.type_component_package_name.bounded_string :=
			et_libraries.to_package_name (ada.directories.base_name (file_name)); 

		function path_and_file_name return string is
		-- returns the path and file name. used for error messages.
			use et_libraries;
		begin
			return "file " & ada.directories.compose (
				to_string (lib_dir), file_name);
		end path_and_file_name;
		
		-- This cursor points to the line being processed (in the list of lines given in "lines"):
		line_cursor : et_pcb.type_lines.cursor := lines.first;

		opening_bracket : constant character := '(';
		closing_bracket : constant character := ')';

		term_char_seq : constant string (1..2) := latin_1.space & closing_bracket;
		term_char_set : character_set := to_set (term_char_seq);

		-- the section prefix is a workaround due to GNAT reserved keywords.
		sec_prefix : constant string (1..4) := "sec_";

		-- These are the keywords used in the package model. They prelude a certain section.
		-- See <https://www.compuphase.com/electronics/LibraryFileFormats.pdf> for more.
		type type_keyword is (
			INIT,	-- initial section before anything is done. does not occur in package model
			SEC_AT,
			SEC_ATTR,
			SEC_ANGLE,
			SEC_CENTER,
			--SEC_CLEARANCE,
			SEC_DESCR,
			SEC_DRILL,
			SEC_EFFECTS,
			SEC_END,
			SEC_FONT,
			SEC_FP_ARC,
			SEC_FP_CIRCLE,
			SEC_FP_LINE,
			SEC_FP_TEXT,
			--SEC_JUSTIFY,
			SEC_LAYER,
			SEC_LAYERS,
			SEC_MODEL,
			SEC_MODULE,
			SEC_OFFSET,
			SEC_PAD,
			SEC_ROTATE,
			SEC_SCALE,
			SEC_SIZE,
			--SEC_SOLDER_MASK_MARGIN,
			SEC_START,
			SEC_TAGS,
			SEC_TEDIT,
			SEC_THICKNESS,
			SEC_WIDTH,
			SEC_XYZ
			);

		argument_length_max : constant positive := 200; -- CS: could become an issue if long URLs used ...
		package type_argument is new generic_bounded_length (argument_length_max);

		-- After a section name, arguments follow. For each section arguments are counted:
		type type_argument_counter is range 0..3;

		function to_string (arg_count : in type_argument_counter) return string is begin
		-- Returns the given argument count as string.
			return trim (type_argument_counter'image (arg_count), left);
		end to_string;			

		-- Type contains the current section name, the parent section name and the pointer to the argument.
		-- The argument counter is reset on entering a section.
		-- It is incremented once an argument is complete.
		type type_section is record
			name 		: type_keyword := INIT;
			parent		: type_keyword := INIT;
			arg_counter	: type_argument_counter := type_argument_counter'first;
		end record;

		section : type_section; -- the section being processed

		-- Since there are numerous subsections we store sections on a stack.
		-- Once a subsection as been entered the previous section is pushed 
		-- on stack (see procedure read_section).
		-- One leaving a subsection the previous section is popped 
		-- from stack (see end of procedure exec_section).
		package sections_stack is new et_general.stack_lifo (max => 20, item => type_section);




		
	
		function to_string (section : in type_keyword) return string is
		-- Converts a section name to a string.
			len : positive := type_keyword'image (section)'last;
		begin
			-- Due to the workaround with the SEC_ prefix (see above), it must be removed from
			-- the section image.
			return to_lower (type_keyword'image (section)(sec_prefix'last+1 ..len));
		end to_string;
	
		function enter_section (section : in type_keyword) return string is begin
			return ("entering section " & to_string (section));
		end enter_section;

		function return_to_section (section : in type_keyword) return string is begin
			return ("returning to section " & to_string (section));
		end return_to_section;

		function process_section (section : in type_keyword) return string is begin
			return ("processing section " & to_string (section));
		end process_section;



	

		
		time_stamp	: type_timestamp; -- temporarily storage of package timestamp
		description	: type_package_description.bounded_string; -- temp. storage of package description
		tags 		: type_package_tags.bounded_string; -- temp. storage of package keywords

		-- The majority of terminals dictates the package technology. The default is THT.
		package_technology : type_assembly_technology := THT;

		-- By default a package is something real (with x,y,z dimension)
		package_appearance : type_package_appearance := REAL;

		line	: type_line;
		arc		: type_arc;
		circle	: type_circle;
		

	-- TERMINALS
		-- Temporarily we need lots of variables for terminal properties.
		-- Later when the final terminals are assigned to the package, these variables
		-- compose the final terminal.
		terminal_name 		: et_libraries.type_terminal_name.bounded_string;
		terminal_technology	: type_assembly_technology;
		terminal_shape_tht 	: type_terminal_shape_tht;
		terminal_shape_smt 	: type_terminal_shape_smt;

		terminal_face 			: et_pcb_coordinates.type_face;
		terminal_drill_size		: type_drill_size; 
		terminal_drill_shape	: type_drill_shape; -- for slotted holes
		terminal_milling_size_x	: type_pad_milling_size;  -- CS use a composite instead ?
		terminal_milling_size_y	: type_pad_milling_size; 
		terminal_drill_offset_x	: type_pad_drill_offset;  -- CS use a composite instead ?
		terminal_drill_offset_y	: type_pad_drill_offset;
	
		terminal_position	: et_pcb_coordinates.type_terminal_position;
		terminal_size_x 	: type_pad_size;  -- CS use a composite instead ?
		terminal_size_y 	: type_pad_size;		

-- 		terminal_copper_width_outer_layers : et_pcb_coordinates.type_distance;
		terminal_copper_width_inner_layers : et_pcb_coordinates.type_distance := 1.0; -- CS load from DRU ?

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_solder_paste, terminal_bot_solder_paste : type_terminal_solder_paste;

		-- This is the flag for the solder paste status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_solder_paste : type_terminal_solder_paste;

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_stop_mask, terminal_bot_stop_mask : type_terminal_stop_mask;

		-- This is the flag for the stop mask status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_stop_mask : type_terminal_stop_mask;

		-- Here we collect all kinds of terminals after they have been built.
		terminals : et_pcb.type_terminals.map;



	-- TEXTS
		text : type_package_text;

		-- Temporarily text placeholders for reference and value are required. 
		placeholder : type_text_placeholder_package;


		
		
	-- CONTAINERS 

		-- NON ELECTRIC !!! COPPER OBJECTS (lines, arcs, circles)
		-- NOTE: Does not include texts as kicad does not allow texts in signal layers.
		copper : et_pcb.type_copper_package_both_sides;

		-- STOP MASK OBJECTS
		stop_mask : et_pcb.type_stop_mask_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated

		-- SOLDER STENCIL OBJECTS
		stencil : et_pcb.type_stencil_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated
	
		-- SILK SCREEN OBJECTS (lines, arcs, circles, texts, text placeholders)
		silk_screen : et_pcb.type_silk_screen_package_both_sides;
	
		-- ASSEMBLY DOC (FAB) OBJECTS (lines, arcs, circles, texts, text placeholders)
		assy_doc : et_pcb.type_assembly_documentation_package_both_sides;

		-- KEEPOUT OBJECTS (lines, arcs, circles)
		keepout : et_pcb.type_keepout_both_sides;

		-- kicad does not allow pcb contours (edge cuts) in a package model.
		-- For this reason, variable pcb_contours is never assigned. Its containers
		-- remain empty:
		pcb_contours			: type_package_pcb_contour;		

		-- kicad does not allow plated pcb contours (plated edge cuts) in a package model.
		-- For this reason, variable pcb_contours_plated is never assigned. Its containers
		-- remain empty:
		pcb_contours_plated 	: type_package_pcb_contour_plated;
		
		route_restrict 			: type_route_restrict_package;
		via_restrict 			: type_via_restrict_package;

		
		procedure init_stop_and_mask is begin
		-- Resets the temporarily status flags of solder paste and stop mask of an SMT terminal.
		-- Does not affect THT terminals (stop mask always open, solder paste never applied).
			terminal_top_solder_paste := type_terminal_solder_paste'first;
			terminal_bot_solder_paste := type_terminal_solder_paste'first;
			terminal_top_stop_mask := type_terminal_stop_mask'first;
			terminal_bot_stop_mask := type_terminal_stop_mask'first;
		end init_stop_and_mask;

		procedure set_stop_and_mask is
		-- From the SMT terminal face, validates the status of stop mask and solder paste.
			use et_pcb_coordinates;
			
			procedure invalid is begin
				log_indentation_reset;
				log (message_error & "contradicting layers in terminal !", console => true);
				log ("face " & to_string (terminal_face), console => true);
				log (" solder paste top " & to_string (terminal_top_solder_paste), console => true);
				log (" solder paste bot " & to_string (terminal_bot_solder_paste), console => true);
				log (" stop mask top    " & to_string (terminal_top_stop_mask), console => true);
				log (" stop mask bot    " & to_string (terminal_bot_stop_mask), console => true);
				raise constraint_error;
			end invalid; 
				
		begin -- set_stop_and_mask
			case terminal_face is
				when TOP => 

					terminal_solder_paste := terminal_top_solder_paste;
					-- CS warning if solder paste not applied ?

					-- A TOP terminal must NOT have BOTTOM paste applied.
					if terminal_bot_solder_paste = APPLIED then
						invalid;
					end if;

					terminal_stop_mask := terminal_top_stop_mask;
					-- CS warning if stop mask closed ?
					
					-- A TOP terminal must have the BOTTOM stop mask OPEN.
					if terminal_bot_stop_mask = OPEN then
						invalid;
					end if;

					
				when BOTTOM =>

					terminal_solder_paste := terminal_bot_solder_paste;
					-- CS warning if solder paste not applied ?
					
					-- A BOTTOM terminal must NOT have TOP paste applied.
					if terminal_top_solder_paste = APPLIED then
						invalid;
					end if;

					terminal_stop_mask := terminal_bot_stop_mask;
					-- CS warning if stop mask closed ?					

					-- A BOTTOM terminal must have the TOP stop mask OPEN.
					if terminal_top_stop_mask = OPEN then
						invalid;
					end if;
			end case;
		end set_stop_and_mask;
		

		-- When a line is fetched from the given list of lines, it is stored in variable
		-- "current_line". CS: The line length is limited by line_length_max and should be increased
		-- if neccessary. 
		-- The character_cursor points to the character being tested or processed in that line.
		line_length_max : constant positive := 300;
		package type_current_line is new generic_bounded_length (line_length_max);
		use type_current_line;
		current_line : type_current_line.bounded_string;
		character_cursor : natural;

		procedure get_next_line is
		-- Fetches a new line from the given list of lines (see header of procedure to_package_model).
		begin
			next (line_cursor);
			if line_cursor /= et_pcb.type_lines.no_element then

				-- Since a single line in container "lines" (where line_cursor points to) is a list 
				-- of strings itself, we convert them first to a fixed string and then to a bounded string.
				current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
				log ("line " & to_string (current_line), log_threshold + 4);
			else
				-- This should never happen:
				log_indentation_reset;
				log (message_error & "in " & path_and_file_name, console => true);
				log (message_error & "no more lines available !", console => true);
				raise constraint_error;
			end if;
		end get_next_line;
		
		procedure next_character is
		-- Updates the cursor position to the position of the next
		-- non_space character starting from the current cursor position.
		-- Fetches a new line if no further characters after current cursor position.
		begin
			character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			while character_cursor = 0 loop
				get_next_line;
				character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			end loop;
		end next_character;

		procedure read_section is 
		-- Stores the section name and current argument counter on sections_stack.
		-- Reads the section name from current cursor position until termination
		-- character or its last character.
			end_of_kw : integer;  -- may become negative if no terminating character present

			procedure invalid_section is
			begin
				log_indentation_reset;
				log (message_error & "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' !", console => true);
				raise constraint_error;
			end invalid_section;
		begin
			-- save previous section on stack
			sections_stack.push (section);

			-- the former actvie section name becomes the parent section name
			section.parent := section.name;
			
			section.arg_counter := 0;
			
			-- get position of last character
			end_of_kw := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

			-- if no terminating character found, end_of_kw assumes length of line
			if end_of_kw = -1 then
				end_of_kw := length (current_line);
			end if;

			-- Compose section name from cursor..end_of_kw.
			-- This is an implicit general test whether the keyword is a valid keyword.
			section.name := type_keyword'value (sec_prefix & slice (current_line, character_cursor, end_of_kw));

			-- This is the validation of a section regarding its parent section.
			-- If an invalid subsection occurs, raise alarm and abort.
			case section.parent is
				when SEC_MODULE =>
					case section.name is
						when SEC_FP_TEXT | SEC_FP_LINE | SEC_FP_ARC | SEC_FP_CIRCLE | SEC_TAGS |
							SEC_MODEL | SEC_PAD | SEC_DESCR | SEC_ATTR | SEC_LAYER | SEC_TEDIT => null;
						when others => invalid_section;
					end case;

				when SEC_FP_TEXT =>
					case section.name is
						when SEC_AT | SEC_LAYER | SEC_EFFECTS => null;
						when others => invalid_section;
					end case;

				when SEC_EFFECTS =>
					case section.name is
						when SEC_FONT => null;
						when others => invalid_section;
					end case;
					
				when SEC_FONT =>
					case section.name is
						when SEC_SIZE | SEC_THICKNESS => null;
						when others => invalid_section;
					end case;

				when SEC_FP_LINE =>
					case section.name is
						when SEC_START | SEC_END | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_FP_ARC =>
					case section.name is
						when SEC_START | SEC_END | SEC_ANGLE | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_FP_CIRCLE =>
					case section.name is
						when SEC_CENTER | SEC_END | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_PAD =>
					case section.name is
						when SEC_AT | SEC_SIZE | SEC_LAYERS | SEC_DRILL => null;
						when others => invalid_section;
					end case;

				when SEC_MODEL =>
					case section.name is
						when SEC_AT | SEC_ROTATE | SEC_SCALE => null;
						when others => invalid_section;
					end case;
					
				when others => null;
			end case;

			
			-- update cursor
			character_cursor := end_of_kw;

			log (enter_section (section.name), log_threshold + 3);

			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & path_and_file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);

						log (message_error & "section '" & slice (current_line, character_cursor, end_of_kw) 
							& "' invalid or not supported yet", console => true);
						raise;
			
		end read_section;
		

		procedure read_arg is
		-- Reads the arguments of a section.
		-- Increments the argument counter after each argument.
		-- Validates the arguments according to the current section.
		-- Leaves the character_cursor at the position of the last character of the argument.
		-- If the argument was enclosed in quotations the character_cursor is left at
		-- the position of the trailing quotation.
			end_of_arg : integer; -- may become negative if no terminating character present

			use type_argument;
			use et_libraries;
			use et_libraries.type_text_content;
			use et_pcb_coordinates;
		
			arg : type_argument.bounded_string; -- here the argument goes temporarily

			procedure invalid_layer is begin
				log_indentation_reset;
				log (message_error & "invalid layer " & to_string (arg), console => true);
				raise constraint_error;
			end invalid_layer;

			procedure too_many_arguments is begin
				log_indentation_reset;
				log (message_error & "too many arguments in section " & to_string (section.name) & " !", console => true);
				log ("excessive argument reads '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end too_many_arguments;

			procedure invalid_fp_text_keyword is begin
				log_indentation_reset;
				log (message_error & "expect keyword '" & keyword_fp_text_reference 
					 & "' or '" & keyword_fp_text_value 
					 & "' or '" & keyword_fp_text_user
					 & "' ! found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_fp_text_keyword;

			procedure invalid_placeholder_reference is begin
				log_indentation_reset;
				log (message_error & "expect reference placeholder '" & placeholder_reference & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_reference;

			procedure invalid_placeholder_value is
			begin
				log_indentation_reset;
				log (message_error & "expect value placeholder '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_value;

			procedure invalid_package_name is
			begin
				log_indentation_reset;
				log (message_error & "expect package name '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_package_name;

			procedure invalid_component_assembly_face is
			begin
				log_indentation_reset;
				log (message_error & "default assembly face " & et_pcb_coordinates.to_string (BOTTOM) 
					 & " found. Must be " & et_pcb_coordinates.to_string (TOP) & " !", console => true);
				raise constraint_error;
			end invalid_component_assembly_face;

			procedure invalid_attribute is
			begin
				log_indentation_reset;
				log (message_error & "invalid attribute !", console => true);
				raise constraint_error;
			end invalid_attribute;

			procedure invalid_section is
			begin
				log_indentation_reset;
				log (message_error & "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' !", console => true);
				raise constraint_error;
			end invalid_section;
				
		begin -- read_arg
			-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
			if element (current_line, character_cursor) = latin_1.quotation then
				-- Read the quotation-wrapped argument (strip quotations)

				-- get position of last character (before trailing quotation)
				end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

				-- if no trailing quotation found -> error
				if end_of_arg = -1 then
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor))
						& latin_1.space & latin_1.quotation & " expected");
						raise constraint_error;
				end if;

				-- compose argument from first character after quotation until end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor + 1, end_of_arg));

				-- update cursor (to position of trailing quotation)
				character_cursor := end_of_arg + 1;
			else
				-- Read the argument from current cursor position until termination
				-- character or its last character.

				-- get position of last character
				end_of_arg := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

				-- if no terminating character found, end_of_arg assumes length of line
				if end_of_arg = -1 then
					end_of_arg := length (current_line);
				end if;

				-- compose argument from cursor..end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor, end_of_arg));

				-- update cursor
				character_cursor := end_of_arg;
			end if;

			-- Argument complete. Increment argument counter of section.
			section.arg_counter := section.arg_counter + 1;
			
			log ("arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), log_threshold + 4);

			-- Validate arguments according to current section and the parent section.
			-- Load variables. When a section closes, the variables are used to build an object. see exec_section.
			case section.name is
				when INIT => raise constraint_error; -- should never happen
				
				when SEC_MODULE =>
					case section.parent is
						when INIT =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) /= to_string (package_name) then
										invalid_package_name;
									end if;
								when others => 
									too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_DESCR =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									description := to_package_description (to_string (arg));
									-- CS check description
								when others => 
									too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_TAGS =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									tags := to_package_tags (to_string (arg));
									-- CS check tags
								when others => 
									too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_TEDIT =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									time_stamp := type_timestamp (to_string (arg));
									et_string_processing.check_timestamp (time_stamp);
								when others => 
									too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_ATTR =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									if to_string (arg) = attribute_technology_smd then
										package_technology := SMT; -- overwrite default (see declarations)
									elsif to_string (arg) = attribute_technology_virtual then
										package_appearance := VIRTUAL;  -- overwrite default (see declarations)
									else
										invalid_attribute;
									end if;
								when others => 
									too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
							
				when SEC_FP_TEXT =>
					case section.parent is
						when SEC_MODULE =>
							text.hidden := false; -- "hide" flag is optionally provided as last argument. if not, default to false
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = keyword_fp_text_reference then
										text.meaning := REFERENCE;
									elsif to_string (arg) = keyword_fp_text_value then
										text.meaning := VALUE;
									elsif to_string (arg) = keyword_fp_text_user then
										text.meaning := USER;
									else
										invalid_fp_text_keyword;
									end if;
									
								when 2 => 
									case text.meaning is
										when REFERENCE => 
											if to_string (arg) /= placeholder_reference then
												invalid_placeholder_reference;
											end if;

										when VALUE =>
											if to_string (arg) /= to_string (package_name) then
												invalid_placeholder_value;
											end if;

										when USER =>
											-- CS length check
											text.content := to_bounded_string (to_string (arg));
											-- CS character check
									end case;
									
								when 3 => 
									if to_string (arg) = keyword_fp_text_hide then
										text.hidden := true;
									end if;
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_CENTER =>
					case section.parent is
						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => circle.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => circle.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_START =>
					case section.parent is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => line.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => line.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => arc.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => arc.center, value => zero_distance);
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;

				when SEC_END =>
					case section.parent is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => line.end_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => line.end_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => arc.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => arc.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => circle.point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => circle.point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_ANGLE =>
					case section.parent is
						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => arc.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_LAYER =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_bot_copper then
										invalid_component_assembly_face;
									elsif to_string (arg) /= layer_top_copper then
										invalid_layer;
									end if;
								when others => too_many_arguments;
							end case;
									
						when SEC_FP_TEXT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										text.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										text.layer := BOT_SILK;
									elsif to_string (arg) = layer_top_assy_doc then
										text.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										text.layer := BOT_ASSY;
									elsif to_string (arg) = layer_top_keepout then
										text.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										text.layer := BOT_KEEP;
									else
										invalid_layer;
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										line.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										line.layer := BOT_SILK;
										
									elsif to_string (arg) = layer_top_assy_doc then
										line.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										line.layer := BOT_ASSY;
										
									elsif to_string (arg) = layer_top_keepout then
										line.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										line.layer := BOT_KEEP;
										
									elsif to_string (arg) = layer_top_copper then
										line.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										line.layer := BOT_COPPER;
										
									elsif to_string (arg) = layer_top_stop_mask then
										line.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										line.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										line.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										line.layer := BOT_PASTE;
									else
										invalid_layer; -- e.g. objects in layer EDGE_CUTS
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										arc.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										arc.layer := BOT_SILK;
										
									elsif to_string (arg) = layer_top_assy_doc then
										arc.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										arc.layer := BOT_ASSY;
										
									elsif to_string (arg) = layer_top_keepout then
										arc.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										arc.layer := BOT_KEEP;

									elsif to_string (arg) = layer_top_copper then
										arc.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										arc.layer := BOT_COPPER;
										
									elsif to_string (arg) = layer_top_stop_mask then
										arc.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										arc.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										arc.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										arc.layer := BOT_PASTE;
									else
										invalid_layer;  -- e.g. objects in layer EDGE_CUTS
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										circle.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										circle.layer := BOT_SILK;
										
									elsif to_string (arg) = layer_top_assy_doc then
										circle.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										circle.layer := BOT_ASSY;
										
									elsif to_string (arg) = layer_top_keepout then
										circle.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										circle.layer := BOT_KEEP;

									elsif to_string (arg) = layer_top_copper then
										circle.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										circle.layer := BOT_COPPER;
										
									elsif to_string (arg) = layer_top_stop_mask then
										circle.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										circle.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										circle.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										circle.layer := BOT_PASTE;
									else
										invalid_layer;  -- e.g. objects in layer EDGE_CUTS
									end if;

								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;

				when SEC_WIDTH =>
					case section.parent is
						when SEC_FP_LINE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									line.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									arc.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									circle.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_SIZE =>
					case section.parent is
						when SEC_FONT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => text.size_x := to_distance (to_string (arg));
								when 2 => text.size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_pad_size (to_distance (to_string (arg)));
									terminal_size_x := to_distance (to_string (arg));
								when 2 => 
									validate_pad_size (to_distance (to_string (arg)));
									terminal_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
					
				when SEC_THICKNESS =>
					case section.parent is
						when SEC_FONT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => text.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_AT =>
					case section.parent is
						when SEC_PAD =>
							set_angle (point => terminal_position, value => zero_angle); -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => terminal_position, value => zero_distance);
								when 3 => 
									set_angle (point => terminal_position, value => to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_TEXT =>
							text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => text.position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => text.position, value => zero_distance);
								when 3 => 
									text.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
							
				when SEC_DRILL =>
					case section.parent is
						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = drill_shape_oval then -- (drill oval 1.2 5.5)
										terminal_drill_shape := SLOTTED;
									else
										terminal_drill_shape := CIRCULAR; -- (drill 2.2)
										validate_drill_size (to_distance (to_string (arg)));
										terminal_drill_size := to_distance (to_string (arg));
									end if;
								when 2 =>
									case terminal_drill_shape is
										when CIRCULAR => too_many_arguments;
										when SLOTTED => terminal_milling_size_x := to_distance (to_string (arg)); -- 1.2
									end case;
								when 3 =>
									case terminal_drill_shape is
										when CIRCULAR => too_many_arguments;
										when SLOTTED => terminal_milling_size_y := to_distance (to_string (arg)); -- 5.5
									end case;
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_OFFSET =>
					case section.parent is
						when SEC_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => terminal_drill_offset_x := to_distance (to_string (arg));
								when 2 => terminal_drill_offset_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
						when others => invalid_section;
					end case;
							
				when SEC_LAYERS => -- applies for terminals exclusively
					case section.parent is
						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;	
								when others => 	
									case terminal_technology is
										when SMT =>

											-- copper
											if to_string (arg) = layer_top_copper then
												terminal_face := TOP;
											elsif to_string (arg) = layer_bot_copper then
												terminal_face := BOTTOM;

											-- solder paste
											elsif to_string (arg) = layer_top_solder_paste then
												terminal_top_solder_paste := APPLIED;
											elsif to_string (arg) = layer_bot_solder_paste then
												terminal_bot_solder_paste := APPLIED;

											-- stop mask
											elsif to_string (arg) = layer_bot_stop_mask then
												terminal_bot_stop_mask := OPEN;
											elsif to_string (arg) = layer_top_stop_mask then
												terminal_top_stop_mask := OPEN;

											else
												invalid_layer;
											end if;

												
										when THT =>

											-- copper and stop mask
											if to_string (arg) = layer_all_copper 
											or to_string (arg) = layer_all_stop_mask then
												null; -- fine
											else
												invalid_layer;
											end if;
											
									end case;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_PAD =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								
								when 1 => null;
									-- CS: check terminal name length
									terminal_name := to_terminal_name (to_string (arg));
									-- CS: check characters
								when 2 =>
									terminal_technology := to_assembly_technology (to_string (arg));
								when 3 =>
									case terminal_technology is
										when SMT => terminal_shape_smt := to_terminal_shape_smt (to_string (arg));
										when THT => terminal_shape_tht := to_terminal_shape_tht (to_string (arg));
									end case;
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_EFFECTS =>
					case section.parent is
						when SEC_FP_TEXT => null; -- CS currently no direct (non-wrapped) arguments follow
						when others => invalid_section;
					end case;

				when SEC_FONT =>
					case section.parent is
						when SEC_EFFECTS => null; -- CS currently no direct (non-wrapped) arguments follow
						when others => invalid_section;
					end case;

				when SEC_FP_LINE | SEC_FP_ARC | SEC_FP_CIRCLE =>
					case section.parent is
						when SEC_MODULE => null; -- CS currently no direct (non-wrapped) arguments follow
						when others => invalid_section;
					end case;

				when SEC_MODEL =>
					case section.parent is
						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS read name of 3d model
								when others => too_many_arguments;
							end case;
						when others => invalid_section;
					end case;
					
				when SEC_ROTATE | SEC_SCALE =>
					case section.parent is
						when SEC_MODULE => null; -- CS currently no direct (non-wrapped) arguments follow
						when others => invalid_section;
					end case;

				when SEC_XYZ =>
					case section.parent is
						when SEC_AT => null; -- CS
						when SEC_SCALE => null; -- CS
						when SEC_ROTATE => null; -- CS
						when others => invalid_section;
					end case;

			end case;
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & path_and_file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;

		end read_arg;

		procedure exec_section is
		-- Performs an operation according to the active section and variables that have been
		-- set earlier (when processing the arguments. see procedure read_arg).
		-- Restores the previous section.
			use et_pcb_coordinates;
			use et_libraries;

			procedure invalid_layer is begin
				log_indentation_reset;
				log (message_error & "invalid layer for this object !", console => true);
				raise constraint_error;
			end invalid_layer;
		
			procedure invalid_layer_reference is begin
				log_indentation_reset;
				log (message_error & "reference placeholder must be in a silk screen layer !", console => true);
				raise constraint_error;
			end invalid_layer_reference;

			procedure invalid_layer_value is begin
				log (message_warning & "value placeholder should be in a fabrication layer !");
			end invalid_layer_value;

			procedure invalid_layer_user is begin
				log_indentation_reset;
				log (message_error & "user text must be in a silk screen or fabrication layer !", console => true);
				raise constraint_error;
			end invalid_layer_user;

			procedure insert_fp_arc is begin
			-- Append the arc to the container corresponding to the layer. Then log the arc properties.
				
				-- compute end point of arc from center, start_point and angle
				arc.end_point := et_pcb_math.arc_end_point (arc.center, arc.start_point, arc.angle);

				-- The angle of the arc and its layer are now discarded
				-- as the arc is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_arc
				-- is formed and appended to the list of silk screen circles.
				case arc.layer is
					when TOP_SILK =>
						silk_screen.top.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_silk_screen_properties (TOP, silk_screen.top.arcs.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_silk_screen_properties (BOTTOM, silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_assy_doc_properties (TOP, assy_doc.top.arcs.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_assy_doc_properties (BOTTOM, assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point));  -- line width discarded because this is keepout
						arc_keepout_properties (TOP, keepout.top.arcs.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point));  -- line width discarded because this is keepout
						arc_keepout_properties (BOTTOM, keepout.bottom.arcs.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_copper_properties (TOP, copper.top.arcs.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_copper_properties (BOTTOM, copper.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						stop_mask.top.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_stop_mask_properties (TOP, stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_stop_mask_properties (BOTTOM, stop_mask.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						stencil.top.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_stencil_properties (TOP, stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						stencil.bottom.arcs.append ((et_pcb.type_arc (arc) with arc.width));
						arc_stencil_properties (BOTTOM, stencil.bottom.arcs.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_arc;

			procedure insert_fp_circle is begin
			-- Append the circle to the container corresponding to the layer. Then log the circle properties.
				
				-- Compute the circle radius from its center and point at circle:
				circle.radius := et_pcb_math.distance (circle.center, circle.point);

				-- The point at the circle and its layer are now discarded
				-- as the circle is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_circle
				-- is formed and appended to the list of silk screen circles.
				case circle.layer is
					when TOP_SILK =>
						silk_screen.top.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_silk_screen_properties (TOP, silk_screen.top.circles.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_silk_screen_properties (BOTTOM, silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_assy_doc_properties (TOP, assy_doc.top.circles.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_assy_doc_properties (BOTTOM, assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.circles.append ((
							center 		=> circle.center,
							radius		=> circle.radius )); -- line width discarded because this is keepout
						circle_keepout_properties (TOP, keepout.top.circles.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.circles.append ((
							center 		=> circle.center,
							radius		=> circle.radius )); -- line width discarded because this is keepout
						circle_keepout_properties (BOTTOM, keepout.bottom.circles.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_copper_properties (TOP, copper.top.circles.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_copper_properties (BOTTOM, copper.bottom.circles.last, log_threshold + 1);

						
					when TOP_STOP =>
						stop_mask.top.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_stop_mask_properties (TOP, stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_stop_mask_properties (BOTTOM, stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						stencil.top.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_stencil_properties (TOP, stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						stencil.bottom.circles.append ((et_pcb.type_circle (circle) with circle.width));
						circle_stencil_properties (BOTTOM, stencil.bottom.circles.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_circle;

			procedure insert_fp_line is begin
			-- Append the line to the container corresponding to the layer. Then log the line properties.
				
				case line.layer is
					when TOP_SILK =>
						silk_screen.top.lines.append ((line.start_point, line.end_point, line.width));
						line_silk_screen_properties (TOP, silk_screen.top.lines.last, log_threshold + 1);

					when BOT_SILK =>
						silk_screen.bottom.lines.append ((line.start_point, line.end_point, line.width));
						line_silk_screen_properties (BOTTOM, silk_screen.bottom.lines.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.lines.append ((line.start_point, line.end_point, line.width));
						line_assy_doc_properties (TOP, assy_doc.top.lines.last, log_threshold + 1);

					when BOT_ASSY =>
						assy_doc.bottom.lines.append ((line.start_point, line.end_point, line.width));
						line_assy_doc_properties (BOTTOM, assy_doc.bottom.lines.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.lines.append ((line.start_point, line.end_point)); -- line width discarded because this is keepout
						line_keepout_properties (TOP, keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						keepout.bottom.lines.append ((line.start_point, line.end_point)); -- line width discarded because this is keepout
						line_keepout_properties (BOTTOM, keepout.bottom.lines.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.lines.append ((line.start_point, line.end_point, line.width));
						line_copper_properties (TOP, copper.top.lines.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.lines.append ((line.start_point, line.end_point, line.width));
						line_copper_properties (BOTTOM, copper.bottom.lines.last, log_threshold + 1);

						
					when TOP_STOP => 
						stop_mask.top.lines.append ((line.start_point, line.end_point, line.width));
						line_stop_mask_properties (TOP, stop_mask.top.lines.last, log_threshold + 1);

					when BOT_STOP => 
						stop_mask.bottom.lines.append ((line.start_point, line.end_point, line.width));
						line_stop_mask_properties (BOTTOM, stop_mask.bottom.lines.last, log_threshold + 1);

						
					when TOP_PASTE => 
						stencil.top.lines.append ((line.start_point, line.end_point, line.width));
						line_stencil_properties (TOP, stencil.top.lines.last, log_threshold + 1);

					when BOT_PASTE => 
						stencil.bottom.lines.append ((line.start_point, line.end_point, line.width));
						line_stencil_properties (BOTTOM, stencil.bottom.lines.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_line;
			

			procedure insert_terminal is 
			-- Insert a terminal in the list "terminals":

				-- this cursor points to the terminal inserted last
				terminal_cursor : et_pcb.type_terminals.cursor;
				-- This flag goes true once a terminal is to be inserted that already exists (by its name).
				terminal_inserted : boolean;
			begin -- insert_terminal
	
				case terminal_technology is
					when THT =>

						if terminal_shape_tht = CIRCULAR then
							terminals.insert (
								key 		=> terminal_name,
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> THT,
												shape 			=> CIRCULAR,
												tht_hole		=> DRILLED,
												width_inner_layers => terminal_copper_width_inner_layers,
												drill_size_cir	=> terminal_drill_size,
												offset_x		=> terminal_drill_offset_x,
												offset_y		=> terminal_drill_offset_y,
												shape_tht		=> terminal_shape_tht,
												position		=> terminal_position
												));
						else -- NON_CIRCULAR
							case terminal_drill_shape is
								when CIRCULAR =>
									terminals.insert (
										key 		=> terminal_name,
										position	=> terminal_cursor,
										inserted	=> terminal_inserted,
										new_item 	=> (
														technology 		=> THT,
														shape			=> NON_CIRCULAR,
														tht_hole		=> DRILLED,
														width_inner_layers => terminal_copper_width_inner_layers,
														drill_size_dri	=> terminal_drill_size,
														shape_tht		=> terminal_shape_tht,
														position		=> terminal_position,
														size_tht_x		=> terminal_size_x,
														size_tht_y		=> terminal_size_y
													));

								when SLOTTED =>
									terminals.insert (
										key 		=> terminal_name,
										position	=> terminal_cursor,
										inserted	=> terminal_inserted,
										new_item 	=> (
														technology 		=> THT,
														shape			=> NON_CIRCULAR,
														tht_hole		=> MILLED,
														width_inner_layers => terminal_copper_width_inner_layers,

														-- The plated millings of the hole is a list of lines.
														-- KiCad does not allow arcs or circles for plated millings.
														millings		=> (lines => contour_milled_rectangle_of_pad
																				(center => terminal_position,
																				size_x => terminal_milling_size_x,
																				size_y => terminal_milling_size_y,
																				offset_x => terminal_drill_offset_x,
																				offset_y => terminal_drill_offset_y),
																				
																			arcs => type_pcb_contour_arcs.empty_list,
																			circles => type_pcb_contour_circles.empty_list),
														shape_tht		=> terminal_shape_tht,
														position		=> terminal_position,
														size_tht_x		=> terminal_size_x,
														size_tht_y		=> terminal_size_y
													));
							end case;

						end if;

						
					when SMT =>

						-- From the SMT terminal face, validate the status of stop mask and solder paste.
						set_stop_and_mask;
						
						if terminal_shape_smt = CIRCULAR then
							terminals.insert (
								key 		=> terminal_name, 
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> SMT,
												shape			=> CIRCULAR,
												tht_hole		=> DRILLED, -- has no meaning here
												shape_smt		=> terminal_shape_smt,
												position		=> terminal_position,
												face 			=> terminal_face,
												stop_mask		=> terminal_stop_mask,
												solder_paste	=> terminal_solder_paste
											));
						else
							terminals.insert (
								key 		=> terminal_name, 
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> SMT,
												shape			=> NON_CIRCULAR,
												tht_hole		=> DRILLED, -- has no meaning here
												shape_smt		=> terminal_shape_smt,
												position		=> terminal_position,
												face 			=> terminal_face,
												stop_mask		=> terminal_stop_mask,
												solder_paste	=> terminal_solder_paste,
												size_smt_x		=> terminal_size_x,
												size_smt_y		=> terminal_size_y
											));
						end if;

						init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)
				end case;

				if terminal_inserted then
					et_pcb.terminal_properties (
						terminal		=> et_pcb.type_terminals.element (terminal_cursor),
						name			=> et_pcb.type_terminals.key (terminal_cursor),
						log_threshold	=> log_threshold + 1);
				else
					log_indentation_reset;
					log (message_error & "duplicated terminal " & to_string (terminal_name) & " !", console => true);
					raise constraint_error;
				end if;

			end insert_terminal;

			procedure insert_fp_text is begin
					
				-- Since there is no alignment information provided, use default values:
				text.alignment := (horizontal => CENTER, vertical => BOTTOM);

				case text.meaning is
					when REFERENCE =>
						placeholder := (et_pcb.type_text (text) with meaning => REFERENCE);
						
						case text.layer is
							when TOP_SILK =>
								silk_screen.top.placeholders.append (placeholder);
								placeholder_silk_screen_properties (TOP, silk_screen.top.placeholders.last, log_threshold + 1);
								
							when BOT_SILK =>
								silk_screen.bottom.placeholders.append (placeholder);
								placeholder_silk_screen_properties (BOTTOM, silk_screen.bottom.placeholders.last, log_threshold + 1);
								
							when others => -- should never happen
								invalid_layer_reference; 
						end case;

					when VALUE =>
						placeholder := (et_pcb.type_text (text) with meaning => VALUE);
						
						case text.layer is
							when TOP_ASSY =>
								assy_doc.top.placeholders.append (placeholder);
								placeholder_assy_doc_properties (TOP, assy_doc.top.placeholders.last, log_threshold + 1);
								
							when BOT_ASSY =>
								assy_doc.bottom.placeholders.append (placeholder);
								placeholder_assy_doc_properties (BOTTOM, assy_doc.bottom.placeholders.last, log_threshold + 1);
								
							when others => -- should never happen
								invalid_layer_value;
						end case;
						
					when USER =>
						case text.layer is
							when TOP_SILK => 
								silk_screen.top.texts.append ((et_pcb.type_text (text) with content => text.content));
								text_silk_screen_properties (TOP, silk_screen.top.texts.last, log_threshold + 1);
								
							when BOT_SILK => 
								silk_screen.bottom.texts.append ((et_pcb.type_text (text) with content => text.content));
								text_silk_screen_properties (BOTTOM, silk_screen.bottom.texts.last, log_threshold + 1);
								
							when TOP_ASSY => 
								assy_doc.top.texts.append ((et_pcb.type_text (text) with content => text.content));
								text_assy_doc_properties (TOP, assy_doc.top.texts.last, log_threshold + 1);
								
							when BOT_ASSY => 
								assy_doc.bottom.texts.append ((et_pcb.type_text (text) with content => text.content));
								text_assy_doc_properties (BOTTOM, assy_doc.bottom.texts.last, log_threshold + 1);
								
							when others -- should never happen. kicad does not allow texts in signal layers 
								=> invalid_layer_user;
						end case;
				end case;
			end insert_fp_text;

			
		begin -- exec_section
			log (process_section (section.name), log_threshold + 4);

			-- CS case construct for section.parent (as with board packages)
			
			case section.name is

				when SEC_TEDIT =>
					log ("timestamp " & string (time_stamp), log_threshold + 1);

				when SEC_DESCR =>
					log (to_string (description), log_threshold + 1);
					
				when SEC_TAGS =>
					log (to_string (tags), log_threshold + 1);

				when SEC_FP_TEXT =>
					insert_fp_text;
	
				when SEC_FP_LINE =>
					insert_fp_line;

				when SEC_FP_ARC =>
					insert_fp_arc;

				when SEC_FP_CIRCLE =>
					insert_fp_circle;
					
				when SEC_PAD =>
					insert_terminal;
					
				when others => null;
			end case;

			-- restore previous section from stack
			section := sections_stack.pop;
			log (return_to_section (section.name), log_threshold + 3);
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & path_and_file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;
			
		end exec_section;
		
		procedure check_placeholders is
		-- Checks if there is at least one placeholder for reference and for value.
		-- CS: validate text sizes and width according to specifications in configuration file
			use et_pcb_coordinates;
			use type_text_placeholders_package;
			cursor 		: type_text_placeholders_package.cursor;
			placeholder : type_text_placeholder_package;
			reference_found, value_found : boolean := false;
		begin
			-- There must be a placeholder for the reference in the top silk screen:
			cursor := silk_screen.top.placeholders.first;
			while cursor /= type_text_placeholders_package.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = REFERENCE then
					reference_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not reference_found then
				log_indentation_reset;
				log (message_error & "in " & path_and_file_name, console => true);
				log (message_error & "no placeholder for component " 
					 & to_string (REFERENCE) 
					 & " found in " & to_string (TOP) & " silk screen !", console => true);
				raise constraint_error;
			end if;

			-- There must be a placeholder for the value in the top assembly documentation:
			cursor := assy_doc.top.placeholders.first;
			while cursor /= type_text_placeholders_package.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = VALUE then
					value_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not value_found then
				log_indentation_reset;
				log (message_error & "in " & path_and_file_name, console => true);
				log (message_error & "no placeholder for component " 
					 & to_string (VALUE) 
					 & " found in " & to_string (TOP) & " assembly documentation !", console => true);
				raise constraint_error;
			end if;
			
		end check_placeholders;

		procedure check_technology is
		-- If the package is REAL, counts the tht and smd terminals. 
		-- Warns operator if the package technology
		-- is not set according to the majority of terminals respectively.
			use et_pcb.type_terminals;
			cursor : et_pcb.type_terminals.cursor := terminals.first;
			tht_count, smt_count : natural := 0; -- the number of THT or SMT terminals

			function number (count : in natural) return string is begin
				return " (" & trim (positive'image (count), left) & "). ";
			end number;
		
		begin -- check_technology
			log ("checking package technology vs. terminal count ...", log_threshold + 1);
			log_indentation_up;
			
			log ("appearance " & to_string (package_appearance), log_threshold + 1);
			
			if package_appearance = REAL then
				log ("assembly technology " & to_string (package_technology), log_threshold + 1);
			
				while cursor /= et_pcb.type_terminals.no_element loop
					case element (cursor).technology is
						when THT => tht_count := tht_count + 1;
						when SMT => smt_count := smt_count + 1;
					end case;
					next (cursor);
				end loop;

				case package_technology is
					when THT =>
						if tht_count < smt_count then
							log (message_warning & "in " & path_and_file_name);
							log (message_warning & "majority of terminals is " & to_string (SMT)
								& number (smt_count)
								& "Package technology should be " & to_string (SMT) & " !");
						end if;

					when SMT =>
						if smt_count < tht_count then
							log (message_warning & "in " & path_and_file_name);
							log (message_warning & "majority of terminals is " & to_string (THT)
								& number (tht_count)
								& "Package technology should be " & to_string (THT) & " !");
						end if;
				end case;

			end if;
			log_indentation_down;
		end check_technology;
		
	begin -- to_package_model
		log ("parsing/building model ...", log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log ("line " & to_string (current_line), log_threshold + 4);
		
		-- get position of first opening bracket
		character_cursor := type_current_line.index (current_line, 1 * opening_bracket);

		init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)

		-- This is the central loop where decisions are made whether to read a section name,
		-- an argument or whether to "execute" a section.
		-- An opening bracket indicates a new (sub)section. A closing bracket indicates that a section
		-- finishes and is to be executed. The loop comes to an end if the sections stack depth 
		-- reaches zero.
		loop
			-- read (sub)section
			<<label_read_section>>
				next_character; -- set character cursor to next character
				read_section;
				next_character; -- set character cursor to next character

				-- if a new subsection starts, read subsection
				if element (current_line, character_cursor) = opening_bracket then goto label_read_section; end if;

			-- read argument
			<<label_read_argument>>
				read_arg;
				next_character; -- set character cursor to next character
			
				-- Test for cb, opening_bracket or other character after argument:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument, the (sub)section ends
					-- and must be executed:
					when closing_bracket => goto label_execute_section;

					-- If another section at a deeper level follows,
					-- read (sub)section:
					when opening_bracket => goto label_read_section;

					-- In case another argument follows, it must be read:
					when others => goto label_read_argument; 
				end case;

			-- execute section
			<<label_execute_section>>
				exec_section;

				-- After executing the section, check the stack depth.
				-- Exit when zero reached (topmost section has been executed).
				if sections_stack.depth = 0 then exit; end if;
				
				next_character; -- set character cursor to next character

				-- Test for cb, opening_bracket or other character after closed section:
				case element (current_line, character_cursor) is

					-- If closing bracket after closed section,
					-- execute parent section:
					when closing_bracket => goto label_execute_section;

					-- If another section at a deeper level follows,
					-- read subsection:
					when opening_bracket => goto label_read_section;

					-- In case an argument follows, it belongs to the parent
					-- section and is to be read:
					when others => goto label_read_argument; 
				end case;
				
		end loop;

		-- check section name. must be top level section
		if section.name /= INIT then -- should never happen
			log_indentation_reset;
			log (message_error & "in " & path_and_file_name, console => true);
			log (message_error & "top level section not closed !", console => true);
			raise constraint_error;
		end if;

		-- check the most relevant placeholders
		check_placeholders;

		-- check assembly technology vs. terminal count
		check_technology;

		-- CS validate description
		
		log_indentation_down;

		-- depending on the attribute we return a real or a virtual package
		case package_appearance is
			when REAL =>
				return (
					appearance				=> REAL,
					package_contours		=> no_contour, -- CS to be filled from 3d model
					pcb_contours			=> pcb_contours,
					pcb_contours_plated 	=> pcb_contours_plated,
					terminals				=> terminals,
					copper					=> copper, -- non electric !
					silk_screen				=> silk_screen,
					keepout					=> keepout,
					stop_mask				=> stop_mask,
					solder_stencil			=> stencil,
					route_restrict 			=> route_restrict,
					via_restrict 			=> via_restrict,
					assembly_documentation 	=> assy_doc,
					time_stamp				=> time_stamp,
					description				=> description,
					technology				=> package_technology
					);

			when VIRTUAL => -- no package_contours
				return (
					appearance				=> VIRTUAL,
					pcb_contours			=> pcb_contours,
					pcb_contours_plated 	=> pcb_contours_plated,
					terminals				=> terminals,
					copper					=> copper, -- non electric !
					silk_screen				=> silk_screen,
					keepout					=> keepout,
					stop_mask				=> stop_mask,
					solder_stencil			=> stencil,
					route_restrict 			=> route_restrict,
					via_restrict 			=> via_restrict,
					assembly_documentation 	=> assy_doc,
					time_stamp				=> time_stamp,
					description				=> description,
					technology				=> package_technology
					);
		end case;
				
	end to_package_model;
	
	procedure read_libraries (
	-- Reads package libraries. Root directory is et_libraries.lib_dir.
	-- The libraries in the container are named after the libraries found in lib_dir.
		log_threshold 	: in et_string_processing.type_log_level) is

		use ada.directories;
		use et_libraries;
		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		-- backup the directory of origin
		use type_directory_name;
		origin_directory : type_directory_name.bounded_string := to_bounded_string (current_directory);
	
		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		library_names : type_directory_entries.list;
		library_name_cursor : type_directory_entries.cursor;

		-- While inserting the libraries the flag library_inserted goes true once
		-- inserting was successuful. It goes false if the library is already in the list.
		-- The library_cursor points to the library in the container package_libraries.
		library_inserted : boolean;
		library_cursor : et_pcb.type_libraries.cursor;

		procedure read_package_names (
		-- Creates empty packages in the package_libraries. The package names are
		-- named after the packages found in the library directories.
			library_name	: in type_full_library_name.bounded_string;
			packages		: in out type_packages_library.map) is

			package_names : type_directory_entries.list;
			package_name_cursor : type_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
			line : type_fields_of_line; -- a line of a package model

			use et_pcb.type_lines;
			lines : et_pcb.type_lines.list; -- all lines of a single package model

		begin -- read_package_names
			log ("reading package names in " & current_directory & " ...", log_threshold + 3);
			log_indentation_up;

			package_names := directory_entries (
								target_directory	=> current_directory, 
								category			=> ada.directories.ordinary_file,
								pattern				=> et_kicad.package_pattern);

			-- show number of package libraries
			if is_empty (package_names) then
				log (message_warning & "library is empty !");
			else
				log ("found" & count_type'image (length (package_names)) & " packages", log_threshold + 4);
			end if;
			
			log_indentation_up;

			package_name_cursor := package_names.first;
			while package_name_cursor /= type_directory_entries.no_element loop
				log (element (package_name_cursor), log_threshold + 5);
				log_indentation_up;
				
				-- open package model file
				open (
					file => library_handle,
					mode => in_file,
					name => element (package_name_cursor)); -- S_0201.kicad_mod

				-- read lines of model file
				set_input (library_handle);
				while not end_of_file loop
-- 					log (get_line);

					-- Store a single line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
								line 	=> get_line,
								number 	=> ada.text_io.line (current_input),
								ifs 	=> latin_1.space); -- fields are separated by space

					-- insert line in container "lines"
					if field_count (line) > 0 then -- we skip empty or commented lines
						append (lines, line);
					end if;
						
				end loop;
				close (library_handle);

				-- From the collected lines the package model can be built and inserted in the 
				-- package list right away:
				type_packages_library.insert (
					container	=> packages,
					key			=> to_package_name (base_name (element (package_name_cursor))), -- S_0201
					new_item	=> to_package_model (
										file_name 		=> element (package_name_cursor), -- S_0201.kicad_mod
										lines			=> lines,
										log_threshold	=> log_threshold + 6));
				
				-- Once the package model file has been read, the collection of lines
				--must be cleared for the next model.
				clear (lines);

				log_indentation_down;
				next (package_name_cursor);
			end loop;

			log_indentation_down;
			log_indentation_down;

			exception
				when event:
					others =>
						log_indentation_reset;
						log (ada.exceptions.exception_message (event), console => true);
						raise;

		end read_package_names;

	
	begin -- read_libraries
		log ("reading package libraries ...", log_threshold);

		-- fetch package library names from lib_dir
		library_names := directory_entries (
							target_directory	=> et_libraries.to_string (et_libraries.lib_dir), 
							category			=> ada.directories.directory,
							pattern				=> et_kicad.package_library_pattern);

		log_indentation_up;

		-- Abort if there are no package libraries. Otherwise loop through the library names
		-- and create the libraries in container package_libraries.
		if is_empty (library_names) then
			log_indentation_reset;
			log (message_error & "no package libraries found !");
			raise constraint_error;
		else
			-- show number of package libraries
			log ("found" & count_type'image (length (library_names)) & " libraries", log_threshold + 1);
			log_indentation_up;

			-- Loop through library names and create the actual libraries in container package_libraries:
			library_name_cursor := library_names.first;
			while library_name_cursor /= type_directory_entries.no_element loop
				log ("reading " & element (library_name_cursor) & " ...", log_threshold + 2);

				-- create the (empty) library
				et_pcb.type_libraries.insert (
					container	=> package_libraries,
-- 					key			=> to_library_name (element (library_name_cursor)),
					key			=> to_full_library_name (
										root_dir => lib_dir,
										lib_name => to_library_name (element (library_name_cursor))),
					inserted	=> library_inserted,
					position	=> library_cursor,
					new_item	=> type_packages_library.empty_map);

				if library_inserted then
					log_indentation_up;
					
					-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
					set_directory (compose (to_string (lib_dir), element (library_name_cursor)));
					
					et_pcb.type_libraries.update_element (
						container	=> package_libraries,
						position	=> library_cursor,
						process		=> read_package_names'access);

					-- change back to directory of origin
					set_directory (et_pcb.to_string (origin_directory));
					log_indentation_down;
				else
					log_indentation_up;
					log ("already loaded -> skipped", log_threshold + 2);
					log_indentation_down;
				end if;
				
				next (library_name_cursor);
			end loop;

			log_indentation_down;
		end if;
		
		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					put_line (ada.exceptions.exception_message (event));
					raise;

	end read_libraries;


	function to_layer_name (name : in string) return type_layer_name.bounded_string is
	begin
		return type_layer_name.to_bounded_string (name);
	end to_layer_name;

	function to_layer_meaning (meaning : in string) return type_layer_meaning is
	begin
		return type_layer_meaning'value (meaning);
	end to_layer_meaning;

	function to_board (
		file_name		: in string; -- pwr_supply.kicad_pcb
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level) 
		return type_board is

		board : type_board; -- to be returned
		
		use et_pcb;
		use et_pcb.type_lines;

		-- This cursor points to the line being processed (in the list of lines given in "lines"):
		line_cursor : et_pcb.type_lines.cursor := lines.first;

		opening_bracket : constant character := '(';
		closing_bracket : constant character := ')';

		term_char_seq : constant string (1..2) := latin_1.space & closing_bracket;
		term_char_set : character_set := to_set (term_char_seq);

		-- the section prefix is a workaround due to GNAT reserved keywords.
		sec_prefix : constant string (1..4) := "sec_";

		-- These are the keywords used in the board file. They prelude a certain section.
		-- See <https://www.compuphase.com/electronics/LibraryFileFormats.pdf> for more.
		type type_keyword is (
			INIT,	-- initial section before anything is done. does not occur in board file
			SEC_ADD_NET,
			SEC_AREA,
			SEC_AUX_AXIS_ORIGIN,
			SEC_AT,
			SEC_ATTR,
			SEC_ANGLE,
			SEC_CENTER,
			SEC_CLEARANCE,
			SEC_DESCR,
			SEC_DRAWINGS,
			SEC_DRILL,
			SEC_DRILLSHAPE,
			SEC_EDGE_WIDTH,
			SEC_EFFECTS,
			SEC_END,
			SEC_EXCLUDEEDGELAYER,
			SEC_FONT,
			SEC_FP_ARC,
			SEC_FP_CIRCLE,
			SEC_FP_LINE,
			SEC_FP_TEXT,
			SEC_GENERAL,
			SEC_GR_ARC,		-- board outlines, edge cuts
			SEC_GR_CIRCLE,	-- board outlines, edge cuts
			SEC_GR_LINE,	-- board outlines, edge cuts
			SEC_HOST,
			SEC_HPGLPENDIAMETER,
			SEC_HPGLPENNUMBER,
			SEC_HPGLPENOVERLAY,
			SEC_HPGLPENSPEED,			
			--SEC_JUSTIFY,
			SEC_KICAD_PCB,
			-- 			SEC_LAYER,
			SEC_LAST_TRACE_WIDTH,
			SEC_LAYER,
			SEC_LAYER_ID, -- "artificially". does not occur in board file (see procedure read_section)
			SEC_LAYERS,
			SEC_LAYERSELECTION,
			SEC_LINEWIDTH,
			SEC_LINKS,
			SEC_MODEL,
			SEC_MODE,
			SEC_MODULE,
			SEC_MODULES,
			SEC_MOD_EDGE_WIDTH,
			SEC_MOD_TEXT_SIZE,
			SEC_MOD_TEXT_WIDTH,
			SEC_MIRROR,
			SEC_NET,
			SEC_NET_CLASS,
			SEC_NETS,
			SEC_NO_CONNECTS,
			SEC_OFFSET,
			SEC_OUTPUTDIRECTORY,
			SEC_OUTPUTFORMAT,
			SEC_PAD,
			SEC_PAD_DRILL,
			SEC_PAD_SIZE,
			SEC_PAD_TO_MASK_CLEARANCE,
			SEC_PADSONSILK,
			SEC_PAGE,
			SEC_PATH,
			SEC_PCBPLOTPARAMS,
			SEC_PCB_TEXT_SIZE,
			SEC_PCB_TEXT_WIDTH,
			SEC_PLOTFRAMEREF,
			SEC_PLOTINVISIBLETEXT,
			SEC_PLOTREFERENCE,
			SEC_PLOTVALUE,
			SEC_PSA4OUTPUT, 
			SEC_PSNEGATIVE,
			SEC_ROTATE,
			SEC_SCALE,
			SEC_SCALESELECTION,
			SEC_SEGMENT_WIDTH,
			SEC_SETUP,
			SEC_SUBTRACTMASKFROMSILK,
			SEC_SIZE,
			--SEC_SOLDER_MASK_MARGIN,
			SEC_START,
			SEC_TAGS,
			SEC_TEDIT,
			SEC_TRACE_CLEARANCE,
			SEC_TRACE_MIN,
			SEC_TRACE_WIDTH,
			SEC_TRACKS,
			SEC_THICKNESS,
			SEC_TSTAMP,
			SEC_USEAUXORIGIN,
			SEC_USEGERBEREXTENSIONS,
			SEC_UVIAS_ALLOWED,
			SEC_UVIA_DIA,
			SEC_UVIA_DRILL,
			SEC_UVIA_MIN_DRILL,
			SEC_UVIA_MIN_SIZE,
			SEC_UVIA_SIZE,
			SEC_VERSION,
			SEC_VIA_DIA,
			SEC_VIA_DRILL,
			SEC_VIA_MIN_DRILL,
			SEC_VIA_MIN_SIZE,
			SEC_VIA_SIZE,
			SEC_VISIBLE_ELEMENTS,
			SEC_VIASONMASK,
			SEC_WIDTH,
			SEC_XYZ,
			SEC_ZONE_45_ONLY,
			SEC_ZONE_CLEARANCE,
			SEC_ZONES
			);
                      		
		
		argument_length_max : constant positive := 200; -- CS: could become an issue if long URLs used ...
		package type_argument is new generic_bounded_length (argument_length_max);

		-- After a section name, arguments follow. For each section arguments are counted:
		type type_argument_counter is range 0..4;

		function to_string (arg_count : in type_argument_counter) return string is begin
		-- Returns the given argument count as string.
			return trim (type_argument_counter'image (arg_count), left);
		end to_string;			

		-- Type contains the current section name, the parent section name and the pointer to the argument.
		-- The argument counter is reset on entering a section.
		-- It is incremented once an argument is complete.
		type type_section is record
			name 		: type_keyword := INIT;
			parent		: type_keyword := INIT;
			arg_counter	: type_argument_counter := type_argument_counter'first;
		end record;

		section : type_section; -- the section being processed

		-- Since there are numerous subsections we store sections on a stack.
		-- Once a subsection as been entered the previous section is pushed 
		-- on stack (see procedure read_section).
		-- One leaving a subsection the previous section is popped 
		-- from stack (see end of procedure exec_section).
		package sections_stack is new et_general.stack_lifo (max => 20, item => type_section);




		
	
		function to_string (section : in type_keyword) return string is
		-- Converts a section name to a string.
			len : positive := type_keyword'image (section)'last;
		begin
			-- Due to the workaround with the SEC_ prefix (see above), it must be removed from
			-- the section image.
			return to_lower (type_keyword'image (section)(sec_prefix'last+1 ..len));
		end to_string;
	
		function enter_section (section : in type_keyword) return string is begin
			return ("entering section " & to_string (section));
		end enter_section;

		function return_to_section (section : in type_keyword) return string is begin
			return ("returning to section " & to_string (section));
		end return_to_section;

		function process_section (section : in type_keyword) return string is begin
			return ("processing section " & to_string (section));
		end process_section;



		-- TEMPORARILY STORAGE PLACES

		
		-- Used when reading the board layers (SEC_LAYERS)
		-- like (0 F.Cu signal) or (31 B.Cu signal) we have those variables.
		layer_id 	: type_layer_id; 
		layer		: type_layer;

		-- NETLIST (things like (net 4 /LED_ANODE) )
		-- NOTE: this has nothing to do with any kicad netlist file !
		netlist_net 		: type_netlist_net;
		
		-- NET CLASSES
		net_class_via_diameter			: et_pcb_coordinates.type_distance;
		net_class_micro_via_diameter	: et_pcb_coordinates.type_distance;
		net_class_via_restring			: et_pcb_coordinates.type_distance;		
		
		net_class_name 	: type_net_class_name.bounded_string;	-- PWR, HIGH_CURRENT, ...
		net_class 		: type_net_class;


		-- PACKAGES
		package_name 			: et_libraries.type_component_package_name.bounded_string;
		package_library_name	: et_libraries.type_library_name.bounded_string;
		package_position		: et_pcb_coordinates.type_package_position;
		package_path			: et_schematic.type_path_to_package; -- the link to the symbol in the schematic like 59F208B2

		-- The majority of terminals dictates the package technology. The default is THT.
		package_technology 	: type_assembly_technology := THT;

		-- By default a package is something real (with x,y,z dimension)
		package_appearance 	: type_package_appearance := REAL;

		package_text 		: type_package_text;
		package_reference 	: et_libraries.type_component_reference := et_schematic.default_component_reference;
		package_value 		: et_libraries.type_component_value.bounded_string;

		package_time_stamp	: type_timestamp; -- temporarily storage of package timestamp
		package_time_edit	: type_timestamp; -- temporarily storage of package time of edit
		package_description	: type_package_description.bounded_string; -- temp. storage of package description
		package_tags 		: type_package_tags.bounded_string; -- temp. storage of package keywords

		package_line 		: type_line;
		package_arc			: type_arc;
		package_circle 		: type_circle;

		package_stop_mask		: et_pcb.type_stop_mask_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated
		
		package_stencil			: et_pcb.type_stencil_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated
		
		package_silk_screen		: et_kicad_pcb.type_silk_screen_package_both_sides;
		package_assy_doc		: et_kicad_pcb.type_assembly_documentation_package_both_sides; -- without placeholders
		package_keepout			: et_pcb.type_keepout_both_sides;
		package_copper			: et_pcb.type_copper_package_both_sides;
		
		-- kicad does not allow pcb contours (edge cuts) in a package model.
		-- For this reason, variable package_pcb_contours is never assigned. Its containers
		-- remain empty:
		package_pcb_contours		: et_pcb.type_package_pcb_contour;

		-- kicad does not allow plated pcb contours (edge cuts) in a package model.
		-- For this reason, variable package_pcb_contours_plated is never assigned. Its containers
		-- remain empty:
		package_pcb_contours_plated	: et_pcb.type_package_pcb_contour_plated;
		
		-- never assigned because kicad does not feature route restrict in a package:
		package_route_restrict		: et_pcb.type_route_restrict_package;

		-- never assigned because kicad does not feature via restrict in a package:
		package_via_restrict		: et_pcb.type_via_restrict_package;

		-- countours of a package as provided by the 3d model:
		package_contours			: et_pcb.type_package_contours; -- CS not assigned yet
		
	-- TERMINALS
		-- Temporarily we need lots of variables for terminal properties.
		-- Later when the final terminals are assigned to the package, these variables
		-- compose the final terminal.
		terminal_name 			: et_libraries.type_terminal_name.bounded_string;
		terminal_technology		: type_assembly_technology;
		terminal_shape_tht 		: type_terminal_shape_tht;
		terminal_shape_smt 		: type_terminal_shape_smt;

		terminal_face 			: et_pcb_coordinates.type_face;
		terminal_drill_size		: type_drill_size; 
		terminal_drill_shape	: type_drill_shape; -- for slotted holes
		terminal_milling_size_x	: type_pad_milling_size;
		terminal_milling_size_y	: type_pad_milling_size;
		terminal_drill_offset_x	: type_pad_drill_offset;
		terminal_drill_offset_y	: type_pad_drill_offset;

		terminal_position	: et_pcb_coordinates.type_terminal_position;
		terminal_size_x 	: type_pad_size;
		terminal_size_y 	: type_pad_size;		

		terminal_net_name	: et_schematic.type_net_name.bounded_string;
		terminal_net_id		: type_net_id_terminal;
	
-- 		terminal_copper_width_outer_layers : et_pcb_coordinates.type_distance;
		terminal_copper_width_inner_layers : et_pcb_coordinates.type_distance := 1.0; -- CS load from DRU ?

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_solder_paste, terminal_bot_solder_paste : type_terminal_solder_paste;

		-- This is the flag for the solder paste status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_solder_paste : type_terminal_solder_paste;

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_stop_mask, terminal_bot_stop_mask : type_terminal_stop_mask;

		-- This is the flag for the stop mask status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_stop_mask : type_terminal_stop_mask;

		-- Here we collect all kinds of terminals after they have been built.
		-- NOTE: This is the type_terminals as specified in et_kicad_pcb ! (includes net names)
		terminals : type_terminals.map;




		-- OBJECTS DRAWN DIRECTLY IN THE BOARD (NON-PACKAGE STUFF)
		board_line 		: type_line;
		board_arc		: type_arc;
		board_circle	: type_circle;

		procedure init_stop_and_mask is begin
		-- Resets the temporarily status flags of solder paste and stop mask of an SMT terminal.
		-- Does not affect THT terminals (stop mask always open, solder paste never applied).
			terminal_top_solder_paste := type_terminal_solder_paste'first;
			terminal_bot_solder_paste := type_terminal_solder_paste'first;
			terminal_top_stop_mask := type_terminal_stop_mask'first;
			terminal_bot_stop_mask := type_terminal_stop_mask'first;
		end init_stop_and_mask;

		procedure set_stop_and_mask is
		-- From the SMT terminal face, validates the status of stop mask and solder paste.
			use et_pcb_coordinates;
			
			procedure invalid is begin
				log_indentation_reset;
				log (message_error & "contradicting layers in terminal !", console => true);
				log ("face " & to_string (terminal_face), console => true);
				log (" solder paste top " & to_string (terminal_top_solder_paste), console => true);
				log (" solder paste bot " & to_string (terminal_bot_solder_paste), console => true);
				log (" stop mask top    " & to_string (terminal_top_stop_mask), console => true);
				log (" stop mask bot    " & to_string (terminal_bot_stop_mask), console => true);
				raise constraint_error;
			end invalid; 
				
		begin -- set_stop_and_mask
			case terminal_face is
				when TOP => 

					terminal_solder_paste := terminal_top_solder_paste;
					-- CS warning if solder paste not applied ?

					-- A TOP terminal must NOT have BOTTOM paste applied.
					if terminal_bot_solder_paste = APPLIED then
						invalid;
					end if;

					terminal_stop_mask := terminal_top_stop_mask;
					-- CS warning if stop mask closed ?
					
					-- A TOP terminal must have the BOTTOM stop mask OPEN.
					if terminal_bot_stop_mask = OPEN then
						invalid;
					end if;

					
				when BOTTOM =>

					terminal_solder_paste := terminal_bot_solder_paste;
					-- CS warning if solder paste not applied ?
					
					-- A BOTTOM terminal must NOT have TOP paste applied.
					if terminal_top_solder_paste = APPLIED then
						invalid;
					end if;

					terminal_stop_mask := terminal_bot_stop_mask;
					-- CS warning if stop mask closed ?					

					-- A BOTTOM terminal must have the TOP stop mask OPEN.
					if terminal_top_stop_mask = OPEN then
						invalid;
					end if;
			end case;
		end set_stop_and_mask;
		
		procedure init_terminal_net_name is begin
		-- Clears the terminal_net_name.
			terminal_net_name := et_schematic.to_net_name ("");
		end init_terminal_net_name;
		
		-- When a line is fetched from the given list of lines, it is stored in variable
		-- "current_line". CS: The line length is limited by line_length_max and should be increased
		-- if neccessary. 
		-- The character_cursor points to the character being tested or processed in that line.
		line_length_max : constant positive := 300;
		package type_current_line is new generic_bounded_length (line_length_max);
		use type_current_line;
		current_line : type_current_line.bounded_string;
		character_cursor : natural;

		procedure get_next_line is
		-- Fetches a new line from the given list of lines (see header of procedure to_board).
		begin
			next (line_cursor);
			if line_cursor /= et_pcb.type_lines.no_element then

				-- Since a single line in container "lines" (where line_cursor points to) is a list 
				-- of strings itself, we convert them first to a fixed string and then to a bounded string.
				current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
				log ("line " & to_string (current_line), log_threshold + 4);
			else
				-- This should never happen:
				log_indentation_reset;
				log (message_error & "in " & file_name, console => true);
				log (message_error & "no more lines available !", console => true);
				raise constraint_error;
			end if;
		end get_next_line;

		procedure next_character is
		-- Updates the cursor position to the position of the next
		-- non_space character starting from the current cursor position.
		-- Fetches a new line if no further characters after current cursor position.
		begin
			character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			while character_cursor = 0 loop
				get_next_line;
				character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
			end loop;
		end next_character;


		procedure read_section is 
		-- Stores the section name and current argument counter on sections_stack.
		-- Reads the section name from current cursor position until termination
		-- character or its last character.
			end_of_kw : integer;  -- may become negative if no terminating character present

			procedure invalid_section is begin
				log_indentation_reset;
				log (message_error & "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' ! (read section)", console => true);
				raise constraint_error;
			end invalid_section;

		begin -- read_section
			-- save previous section on stack
			sections_stack.push (section);

			-- the former actvie section name becomes the parent section name
			section.parent := section.name;
			
			section.arg_counter := 0;
			
			-- get position of last character
			end_of_kw := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

			-- if no terminating character found, end_of_kw assumes length of line
			if end_of_kw = -1 then
				end_of_kw := length (current_line);
			end if;

			-- Usually a section name starts with a letter. In this case
			-- compose section name from cursor..end_of_kw.
			-- This is an implicit general test whether the keyword is a valid keyword.
			
			-- If the section name starts with a digit (like 31 B.Cu signal), it is about a layer id 
			-- in parent section "layers".
			-- NOTE: The section name becomes SEC_LAYER_ID (this section is "artificially" and does
			-- not occur in the board file. Why this approach ? A section must have a name.
			-- So we invent an artificial name for the section that contains the particular layer id
			-- layer name and meaning like "(31 B.Cu signal)".
			if is_letter (element (current_line, character_cursor)) then
				section.name := type_keyword'value (sec_prefix & slice (current_line, character_cursor, end_of_kw));
			else
				if section.parent = SEC_LAYERS then
					-- CS: more careful range check
					layer_id := type_layer_id'value (slice (current_line, character_cursor, end_of_kw));
					-- NOTE: The layer_id must be set here and further processed in procedure read_arg.
					section.name := SEC_LAYER_ID; -- see comments above
				else
					log_indentation_reset;
					log (message_error & "expect subsection name !", console => true);
					raise constraint_error;
				end if;
			end if;
			
			-- This is the validation of a section regarding its parent section.
			-- If an invalid subsection occurs, raise alarm and abort.
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION | SEC_HOST | SEC_GENERAL | SEC_PAGE |
							SEC_LAYERS | SEC_SETUP | SEC_NET | SEC_NET_CLASS |
							SEC_MODULE | SEC_GR_LINE | SEC_GR_ARC | SEC_GR_CIRCLE => null;
						when others => invalid_section;
					end case;

				when SEC_SETUP =>
					case section.name is
						when SEC_LAST_TRACE_WIDTH | SEC_TRACE_CLEARANCE | SEC_ZONE_CLEARANCE | SEC_ZONE_45_ONLY |
							SEC_TRACE_MIN | SEC_SEGMENT_WIDTH | SEC_EDGE_WIDTH | SEC_VIA_SIZE | SEC_VIA_DRILL |
							SEC_VIA_MIN_SIZE | SEC_VIA_MIN_DRILL | SEC_UVIA_SIZE | SEC_UVIA_DRILL | SEC_UVIAS_ALLOWED |
							SEC_UVIA_MIN_SIZE | SEC_UVIA_MIN_DRILL | SEC_PCB_TEXT_WIDTH | SEC_PCB_TEXT_SIZE |
							SEC_MOD_EDGE_WIDTH | SEC_MOD_TEXT_SIZE | SEC_MOD_TEXT_WIDTH |
							SEC_PAD_SIZE | SEC_PAD_DRILL | SEC_PAD_TO_MASK_CLEARANCE | SEC_AUX_AXIS_ORIGIN |
							SEC_VISIBLE_ELEMENTS | SEC_PCBPLOTPARAMS => null;
						when others => invalid_section;
					end case;

				when SEC_PCBPLOTPARAMS =>
					case section.name is
						when SEC_LAYERSELECTION | SEC_USEGERBEREXTENSIONS | SEC_EXCLUDEEDGELAYER | SEC_LINEWIDTH |
							SEC_PLOTFRAMEREF | SEC_VIASONMASK | SEC_MODE | SEC_USEAUXORIGIN | SEC_HPGLPENNUMBER |
							SEC_HPGLPENSPEED | SEC_HPGLPENDIAMETER | SEC_HPGLPENOVERLAY | SEC_PSNEGATIVE |
							SEC_PSA4OUTPUT | SEC_PLOTREFERENCE | SEC_PLOTVALUE | SEC_PLOTINVISIBLETEXT |
							SEC_PADSONSILK | SEC_SUBTRACTMASKFROMSILK | SEC_OUTPUTFORMAT | SEC_MIRROR |
							SEC_DRILLSHAPE | SEC_SCALESELECTION | SEC_OUTPUTDIRECTORY => null;
						when others => invalid_section;
					end case;

				when SEC_NET_CLASS =>
					case section.name is
						when SEC_CLEARANCE | SEC_TRACE_WIDTH | SEC_VIA_DIA | SEC_VIA_DRILL |
							SEC_UVIA_DIA | SEC_UVIA_DRILL | SEC_ADD_NET => null;
						when others => invalid_section;
					end case;

				when SEC_MODULE =>
					case section.name is
						when SEC_FP_TEXT | SEC_FP_LINE | -- SEC_FP_ARC | -- SEC_FP_CIRCLE
							SEC_PAD | SEC_LAYER | SEC_TEDIT | SEC_DESCR | SEC_TSTAMP | SEC_ATTR | SEC_TAGS |
							SEC_AT | SEC_PATH | SEC_MODEL => null;
						when others => invalid_section;
					end case;

				when SEC_FP_TEXT =>
					case section.name is
						when SEC_AT | SEC_LAYER | SEC_EFFECTS => null;
						when others => invalid_section;
					end case;

				when SEC_EFFECTS =>
					case section.name is
						when SEC_FONT => null;
						when others => invalid_section;
					end case;
					
				when SEC_FONT =>
					case section.name is
						when SEC_SIZE | SEC_THICKNESS => null;
						when others => invalid_section;
					end case;

				when SEC_FP_LINE =>
					case section.name is
						when SEC_START | SEC_END | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_FP_ARC =>
					case section.name is
						when SEC_START | SEC_END | SEC_ANGLE | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_FP_CIRCLE =>
					case section.name is
						when SEC_CENTER | SEC_END | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_GENERAL =>
					case section.name is
						when SEC_LINKS | SEC_NO_CONNECTS | SEC_AREA | SEC_THICKNESS | SEC_DRAWINGS |
							SEC_TRACKS | SEC_ZONES | SEC_MODULES | SEC_NETS => null;
						when others => invalid_section;
					end case;

				when SEC_LAYERS =>
					case section.name is
						when SEC_LAYER_ID => null;
						when others => invalid_section;
					end case;
					
				when SEC_PAD =>
					case section.name is
						when SEC_AT | SEC_SIZE | SEC_LAYERS | SEC_DRILL | SEC_NET => null;
						when others => invalid_section;
					end case;

				when SEC_MODEL =>
					case section.name is
						when SEC_AT | SEC_ROTATE | SEC_SCALE => null;
						when others => invalid_section;
					end case;

				-- parent section
				when SEC_GR_CIRCLE =>
					case section.name is
						when SEC_CENTER | SEC_END | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

				when SEC_GR_ARC | SEC_GR_LINE =>
					case section.name is
						when SEC_START | SEC_END | SEC_ANGLE | SEC_LAYER | SEC_WIDTH => null;
						when others => invalid_section;
					end case;

					
				when others => null;
			end case;

			
			-- update cursor
			character_cursor := end_of_kw;

			log (enter_section (section.name), log_threshold + 3);

			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);

						log (message_error & "section '" & slice (current_line, character_cursor, end_of_kw) 
							& "' invalid or not supported yet", console => true);
						raise;
			
		end read_section;
		

		procedure read_arg is
		-- Reads the arguments of a section.
		-- Increments the argument counter after each argument.
		-- Validates the arguments according to the current section.
		-- Leaves the character_cursor at the position of the last character of the argument.
		-- If the argument was enclosed in quotations the character_cursor is left at
		-- the position of the trailing quotation.
			end_of_arg : integer; -- may become negative if no terminating character present

			use type_argument;
			use et_libraries;
			use et_libraries.type_text_content;
			use et_pcb_coordinates;
		
			arg : type_argument.bounded_string; -- here the argument goes temporarily

			procedure invalid_layer is begin
				log_indentation_reset;
				log (message_error & "invalid layer " & to_string (arg), console => true);
				raise constraint_error;
			end invalid_layer;

			procedure too_many_arguments is begin
				log_indentation_reset;
				log (message_error & "too many arguments in section " & to_string (section.name) & " !", console => true);
				log ("excessive argument reads '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end too_many_arguments;

			procedure invalid_fp_text_keyword is begin
				log_indentation_reset;
				log (message_error & "expect keyword '" & keyword_fp_text_reference 
					 & "' or '" & keyword_fp_text_value 
					 & "' or '" & keyword_fp_text_user
					 & "' ! found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_fp_text_keyword;

			procedure invalid_attribute is begin
				log_indentation_reset;
				log (message_error & "invalid attribute !", console => true);
				raise constraint_error;
			end invalid_attribute;

			procedure invalid_section is begin
				log_indentation_reset;
				log (message_error & "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' ! (read argument)", console => true);
				raise constraint_error;
			end invalid_section;

			procedure invalid_file_format is begin
				log_indentation_reset;
				log (message_error & "invalid file format ! Expect format version " & pcb_file_format_version_4 & " !",
					 console => true);
				raise constraint_error;
			end invalid_file_format;

			procedure invalid_host_name is begin
				log_indentation_reset;
				log (message_error & "invalid host name ! Expect " & host_name_pcbnew & " !",
					 console => true);
				raise constraint_error;
			end invalid_host_name;

			procedure invalid_pcbnew_version is begin
				log_indentation_reset;
				log (message_error & "invalid " & host_name_pcbnew & " version ! Expect " & pcb_new_version_4_0_7 & " !",
					 console => true);
				raise constraint_error;
			end invalid_pcbnew_version;
			
		begin -- read_arg
			-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
			if element (current_line, character_cursor) = latin_1.quotation then
				-- Read the quotation-wrapped argument (strip quotations)

				-- get position of last character (before trailing quotation)
				end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

				-- if no trailing quotation found -> error
				if end_of_arg = -1 then
					log_indentation_reset;
					log (message_error & affected_line (element (line_cursor))
						& latin_1.space & latin_1.quotation & " expected");
						raise constraint_error;
				end if;

				-- compose argument from first character after quotation until end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor + 1, end_of_arg));

				-- update cursor (to position of trailing quotation)
				character_cursor := end_of_arg + 1;
			else
				-- Read the argument from current cursor position until termination
				-- character or its last character.

				-- get position of last character
				end_of_arg := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

				-- if no terminating character found, end_of_arg assumes length of line
				if end_of_arg = -1 then
					end_of_arg := length (current_line);
				end if;

				-- compose argument from cursor..end_of_arg
				arg := to_bounded_string (slice (current_line, character_cursor, end_of_arg));

				-- update cursor
				character_cursor := end_of_arg;
			end if;

			-- Argument complete. Increment argument counter of section.
			section.arg_counter := section.arg_counter + 1;
			
			log ("arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), log_threshold + 4);

			-- Validate arguments according to current section and the parent section.
			-- Load variables. When a section closes, the variables are used to build an object. see exec_section.
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) /= pcb_file_format_version_4 then
										invalid_file_format;
									end if;
								when others => too_many_arguments;
							end case;

						when SEC_HOST =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) /= host_name_pcbnew then
										invalid_host_name;
									end if;
								when 2 =>
									if to_string (arg) /= pcb_new_version_4_0_7 then
										invalid_pcbnew_version;
									end if;
								when others => too_many_arguments;
							end case;

						when SEC_PAGE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.paper_size := et_general.to_paper_size (to_string (arg)); -- A4
								when others => too_many_arguments;
							end case;

						when SEC_NET_CLASS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => net_class_name := type_net_class_name.to_bounded_string (to_string (arg)); -- PWR, HIGH_CURRENT, ...
								when 2 => net_class.description := type_net_class_description.to_bounded_string (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 => netlist_net.id := type_net_id'value (to_string (arg));
								when 2 => netlist_net.name := et_schematic.to_net_name (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MODULE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => -- break down something like bel_ic:S_SO14 into package and lib name
									package_library_name := et_kicad.library_name (to_string (arg));
									package_name := et_kicad.package_name (to_string (arg));
									-- CS make sure library and package exist
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;

				-- parent section
				when SEC_NET_CLASS => 
					case section.name is
						when SEC_CLEARANCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_signal_clearance (to_distance (to_string (arg)));
									net_class.clearance := (to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_TRACE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_signal_width (to_distance (to_string (arg)));
									net_class.signal_width_min := (to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_VIA_DIA =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									net_class_via_diameter := (to_distance (to_string (arg)));
									-- validation takes place once the class section is read completely
								when others => too_many_arguments;
							end case;

						when SEC_VIA_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_drill_size (to_distance (to_string (arg)));
									net_class.via_drill_min := (to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_DIA =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									net_class_micro_via_diameter := (to_distance (to_string (arg)));
									-- validation takes place once the class section is read completely
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_drill_size (to_distance (to_string (arg)));
									net_class.micro_via_drill_min := (to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_ADD_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									net_class.net_names.append (et_schematic.to_net_name (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_MODULE =>
					case section.name is
						when SEC_LAYER => 
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) = layer_bot_copper then
										set_face (face => BOTTOM, position => package_position);
									elsif to_string (arg) /= layer_top_copper then
										set_face (face => TOP, position => package_position);
									end if;
								when others => too_many_arguments;
							end case;
							
						when SEC_TEDIT =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									package_time_edit := type_timestamp (to_string (arg));
									et_string_processing.check_timestamp (package_time_edit);
								when others => too_many_arguments;
							end case;

						when SEC_TSTAMP =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									package_time_stamp := type_timestamp (to_string (arg));
									et_string_processing.check_timestamp (package_time_stamp);
								when others => too_many_arguments;
							end case;

						when SEC_DESCR =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									package_description := to_package_description (to_string (arg));
									-- CS check length and characters
								when others => too_many_arguments;
							end case;

						when SEC_TAGS =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									package_tags := to_package_tags (to_string (arg));
									-- CS check length and characters
								when others => too_many_arguments;
							end case;
							
						when SEC_AT =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									set_point (axis => X, point => package_position, value => to_distance (to_string (arg)));
								when 2 =>
									set_point (axis => Y, point => package_position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_position, value => zero_distance);
								when 3 =>
									set_angle (point => package_position, value => to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_PATH =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- the path is given like this /59F207B1. The forward slash must be removed:
									package_path := et_schematic.type_path_to_package (
										to_string (arg)(2..package_path'length + 1));
								when others => too_many_arguments;
							end case;

						when SEC_ATTR =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									if to_string (arg) = attribute_technology_smd then
										package_technology := SMT; -- overwrite default (see declarations)
									elsif to_string (arg) = attribute_technology_virtual then
										package_appearance := VIRTUAL;  -- overwrite default (see declarations)
									else
										invalid_attribute;
									end if;
								when others => too_many_arguments;
							end case;

						when SEC_FP_TEXT =>
							package_text.hidden := false; -- "hide" flag is optionally provided as last argument. if not, default to false
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = keyword_fp_text_reference then
										package_text.meaning := REFERENCE;
									elsif to_string (arg) = keyword_fp_text_value then
										package_text.meaning := VALUE;
									elsif to_string (arg) = keyword_fp_text_user then
										package_text.meaning := USER;
									else
										invalid_fp_text_keyword;
									end if;
									
								when 2 => 
									case package_text.meaning is
										when REFERENCE => 
											-- The reference (like R45) is both the text content and the reference itself.
										
											-- CS length check
											package_text.content := to_bounded_string (to_string (arg));
											-- CS character check

											package_reference := et_schematic.to_component_reference (to_string (arg));
											
										when VALUE =>
											-- The value (like 220R) is both the text content and the value itself.
										
											-- CS length check
											package_text.content := to_bounded_string (to_string (arg));
											-- CS character check

											check_value_length (to_string (arg));
											package_value := et_libraries.to_value (to_string (arg));
											check_value_characters (package_value);
											
										when USER =>
											-- CS length check
											package_text.content := to_bounded_string (to_string (arg));
											-- CS character check
									end case;
									
								when 3 => 
									if to_string (arg) = keyword_fp_text_hide then
										package_text.hidden := true;
									end if;
									
								when others => too_many_arguments;
							end case;

						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null;
									-- CS: check terminal name length
									terminal_name := to_terminal_name (to_string (arg));
									-- CS: check characters
								when 2 =>
									terminal_technology := to_assembly_technology (to_string (arg));
								when 3 =>
									case terminal_technology is
										when SMT => terminal_shape_smt := to_terminal_shape_smt (to_string (arg));
										when THT => terminal_shape_tht := to_terminal_shape_tht (to_string (arg));
									end case;
								when others => too_many_arguments;
							end case;

						when SEC_MODEL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS path to 3d model
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FP_TEXT =>
					case section.name is
						when SEC_AT =>
							package_text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_text.position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_text.position, value => zero_distance);
								when 3 => 
									package_text.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										package_text.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										package_text.layer := BOT_SILK;
									elsif to_string (arg) = layer_top_assy_doc then
										package_text.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										package_text.layer := BOT_ASSY;
									elsif to_string (arg) = layer_top_keepout then
										package_text.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										package_text.layer := BOT_KEEP;
									else
										invalid_layer; -- CS copper layers ?
									end if;

								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FONT =>	
					case section.name is
						when SEC_SIZE => 
							case section.arg_counter is
								when 0 => null;
								when 1 => package_text.size_x := to_distance (to_string (arg));
								when 2 => package_text.size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_THICKNESS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => package_text.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FP_LINE =>
					case section.name is
						when SEC_START =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_line.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_line.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_line.end_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_line.end_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										package_line.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										package_line.layer := BOT_SILK;

									elsif to_string (arg) = layer_top_assy_doc then
										package_line.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										package_line.layer := BOT_ASSY;
									
									elsif to_string (arg) = layer_top_keepout then
										package_line.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										package_line.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_copper then
										package_line.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										package_line.layer := BOT_COPPER;

									elsif to_string (arg) = layer_top_stop_mask then
										package_line.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										package_line.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										package_line.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										package_line.layer := BOT_PASTE;
									else
										invalid_layer;
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									package_line.width := to_distance (to_string (arg));
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FP_CIRCLE =>
					case section.name is
						when SEC_CENTER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_circle.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_circle.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_circle.point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_circle.point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										package_circle.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										package_circle.layer := BOT_SILK;
									
									elsif to_string (arg) = layer_top_assy_doc then
										package_circle.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										package_circle.layer := BOT_ASSY;

									elsif to_string (arg) = layer_top_keepout then
										package_circle.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										package_circle.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_copper then
										package_circle.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										package_circle.layer := BOT_COPPER;

									elsif to_string (arg) = layer_top_stop_mask then
										package_circle.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										package_circle.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										package_circle.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										package_circle.layer := BOT_PASTE;
									else
										invalid_layer;
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									package_circle.width := to_distance (to_string (arg));
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FP_ARC =>
					case section.name is
						when SEC_START =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_arc.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_arc.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => package_arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => package_arc.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => package_arc.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_ANGLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => package_arc.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;
								
						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = layer_top_silk_screen then
										package_arc.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										package_arc.layer := BOT_SILK;
									
									elsif to_string (arg) = layer_top_assy_doc then
										package_arc.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										package_arc.layer := BOT_ASSY;
									
									elsif to_string (arg) = layer_top_keepout then
										package_arc.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										package_arc.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_copper then
										package_arc.layer := TOP_COPPER;
									elsif to_string (arg) = layer_bot_copper then
										package_arc.layer := BOT_COPPER;

									elsif to_string (arg) = layer_top_stop_mask then
										package_arc.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										package_arc.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										package_arc.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										package_arc.layer := BOT_PASTE;
									else
										invalid_layer;
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_general_line_width (to_distance (to_string (arg)));
									package_arc.width := to_distance (to_string (arg));
									
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				-- parent_section
				when SEC_PAD =>
					case section.name is
						when SEC_SIZE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_pad_size (to_distance (to_string (arg)));
									terminal_size_x := to_distance (to_string (arg));
								when 2 => 
									validate_pad_size (to_distance (to_string (arg)));
									terminal_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_AT =>
							--terminal_angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set_angle (point => terminal_position, value => zero_angle);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => terminal_position, value => zero_distance);
								when 3 => 
									set_angle (point => terminal_position, value => to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;
							
						when SEC_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = drill_shape_oval then -- (drill oval 1.2 5.5)
										terminal_drill_shape := SLOTTED;
									else
										terminal_drill_shape := CIRCULAR; -- (drill 2.2)
										validate_drill_size (to_distance (to_string (arg)));
										terminal_drill_size := to_distance (to_string (arg));
									end if;
								when 2 =>
									case terminal_drill_shape is
										when CIRCULAR => too_many_arguments;
										when SLOTTED => terminal_milling_size_x := to_distance (to_string (arg)); -- 1.2
									end case;
								when 3 =>
									case terminal_drill_shape is
										when CIRCULAR => too_many_arguments;
										when SLOTTED => terminal_milling_size_y := to_distance (to_string (arg)); -- 5.5
									end case;
									
								when others => too_many_arguments;
							end case;

						when SEC_LAYERS =>
							case section.arg_counter is
								when 0 => null;	
								when others => 	
									case terminal_technology is
										when SMT =>

											-- copper
											if to_string (arg) = layer_top_copper then
												terminal_face := TOP;
											elsif to_string (arg) = layer_bot_copper then
												terminal_face := BOTTOM;

											-- solder paste
											elsif to_string (arg) = layer_top_solder_paste then
												terminal_top_solder_paste := APPLIED;
											elsif to_string (arg) = layer_bot_solder_paste then
												terminal_bot_solder_paste := APPLIED;

											-- stop mask
											elsif to_string (arg) = layer_bot_stop_mask then
												terminal_bot_stop_mask := OPEN;
											elsif to_string (arg) = layer_top_stop_mask then
												terminal_top_stop_mask := OPEN;

											else
												invalid_layer;
											end if;

												
										when THT =>

											-- copper and stop mask
											if to_string (arg) = layer_all_copper 
											or to_string (arg) = layer_all_stop_mask then
												null; -- fine
											else
												invalid_layer;
											end if;
											
									end case;
							end case;

						when SEC_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 => terminal_net_id := type_net_id_terminal'value (to_string (arg));
								when 2 => terminal_net_name := et_schematic.to_net_name (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_DRILL =>
					case section.name is
						when SEC_OFFSET =>
							case section.arg_counter is
								when 0 => null;
								when 1 => terminal_drill_offset_x := to_distance (to_string (arg));
								when 2 => terminal_drill_offset_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
						when others => invalid_section;
					end case;

				-- parent section
				when SEC_LAYERS =>
					case section.name is
						when SEC_LAYER_ID =>  -- NOTE: this is an "artificial" layer. See procedure read_section.
							-- layer_id already set (see procedure read_section)
							case section.arg_counter is
								when 0 => null;
								when 1 => layer.name := to_layer_name (to_string (arg));
								when 2 => layer.meaning := to_layer_meaning (to_string (arg));
								when others => too_many_arguments;
							end case;
						when others => invalid_section;
					end case;
						
				-- parent section
				when SEC_SETUP =>
					case section.name is
						when SEC_LAST_TRACE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.last_trace_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_TRACE_CLEARANCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.trace_clearance := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_ZONE_CLEARANCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.zone_clearance := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_ZONE_45_ONLY =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.zone_45_only := type_zone_45_only'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_TRACE_MIN =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.trace_min := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_SEGMENT_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.segment_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_EDGE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.edge_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_VIA_SIZE => -- regular vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.via_size := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_VIA_DRILL => -- regular vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.via_drill := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_VIA_MIN_SIZE => -- regular vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.via_min_size := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_VIA_MIN_DRILL => -- regular vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.via_min_drill := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_SIZE => -- micro vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.micro_via_size := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_DRILL => -- micro vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.micro_via_drill := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_UVIAS_ALLOWED => -- micro vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.micro_vias_allowed := to_micro_vias_allowed (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_MIN_SIZE => -- micro vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.micro_via_min_size := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_UVIA_MIN_DRILL => -- micro vias
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.micro_via_min_drill := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PCB_TEXT_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.pcb_text_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PCB_TEXT_SIZE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.pcb_text_size_x := to_distance (to_string (arg));
								when 2 => board.setup.pcb_text_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MOD_EDGE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.module_edge_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_MOD_TEXT_SIZE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.module_text_size_x := to_distance (to_string (arg));
								when 2 => board.setup.module_text_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MOD_TEXT_WIDTH => -- line width
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.module_text_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PAD_SIZE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.pad_size_x := to_distance (to_string (arg));
								when 2 => board.setup.pad_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PAD_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.pad_drill := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PAD_TO_MASK_CLEARANCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.stop_mask_expansion := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_AUX_AXIS_ORIGIN =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.aux_axis_origin_x := to_distance (to_string (arg));
								when 2 => board.setup.aux_axis_origin_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_VISIBLE_ELEMENTS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.setup.visible_elements := type_visible_elements (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_PCBPLOTPARAMS =>
					case section.name is
						when SEC_LAYERSELECTION =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.layer_selection := type_plot_layer_selection_string.to_bounded_string (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_USEGERBEREXTENSIONS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.user_gerber_extensions := type_plot_user_gerber_extensions'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_EXCLUDEEDGELAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.exclude_edge_layer := type_plot_exclude_edge_layer'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_LINEWIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.line_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PLOTFRAMEREF =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.frame_ref := type_plot_frame_ref'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_VIASONMASK =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.vias_on_mask := type_plot_vias_on_mask'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MODE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.fill_mode := type_plot_fill_mode'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_USEAUXORIGIN =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.use_aux_origin := type_plot_use_aux_origin'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_HPGLPENNUMBER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.hpgl_pen_number := type_plot_hpgl_pen_number'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_HPGLPENSPEED =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.hpgl_pen_speed := type_plot_hpgl_pen_speed'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_HPGLPENDIAMETER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.hpgl_pen_diameter := mil_to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_HPGLPENOVERLAY =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.hpgl_pen_overlay := mil_to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PSNEGATIVE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.ps_negative := type_plot_ps_negative'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_PSA4OUTPUT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.psa_4_output := type_plot_psa_4_output'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PLOTREFERENCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.reference := type_plot_reference'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PLOTVALUE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.value := type_plot_value'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_PLOTINVISIBLETEXT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.invisble_text := type_plot_invisible_text'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PADSONSILK =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.pads_on_silk := type_pads_on_silk'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_SUBTRACTMASKFROMSILK =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.subtract_mask_from_silk := type_plot_subtract_mask_from_silk'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_OUTPUTFORMAT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.output_format := type_plot_output_format'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MIRROR =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.mirror := type_plot_mirror'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_DRILLSHAPE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.drill_shape := type_plot_drill_shape'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_SCALESELECTION =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.scale_selection := type_plot_scale_selection'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_OUTPUTDIRECTORY =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.plot.output_directory := to_plot_output_directory (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
							
				-- parent section
				when SEC_GENERAL =>
					case section.name is
						when SEC_LINKS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.links := type_general_links'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_NO_CONNECTS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.no_connects := type_general_no_connects'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_AREA =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.area_x1 := to_distance (to_string (arg));
								when 2 => board.general.area_y1 := to_distance (to_string (arg));								
								when 3 => board.general.area_x2 := to_distance (to_string (arg));
								when 4 => board.general.area_y2 := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_THICKNESS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.thickness := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_DRAWINGS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.drawings := type_general_drawings'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_TRACKS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.tracks := type_general_tracks'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_ZONES =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.zones := type_general_zones'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_MODULES =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.modules := type_general_modules'value (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_NETS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board.general.nets := type_net_id_terminal'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_GR_ARC =>
					case section.name is
						when SEC_START =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_arc.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_arc.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_arc.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_arc.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_ANGLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board_arc.angle := to_angle (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) = layer_top_silk_screen then
										board_arc.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										board_arc.layer := BOT_SILK;

									elsif to_string (arg) = layer_top_assy_doc then
										board_arc.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										board_arc.layer := BOT_ASSY;
									
									elsif to_string (arg) = layer_top_keepout then
										board_arc.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										board_arc.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_stop_mask then
										board_arc.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										board_arc.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										board_arc.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										board_arc.layer := BOT_PASTE;

									elsif to_string (arg) = layer_edge_cuts then
										board_arc.layer := EDGE_CUTS;
									else
										invalid_layer;
									end if;

								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									-- NOTE: The width of the contour does not matter for the manufacturer.
									-- But for the sake of completeness we check the line width anyway.
									-- The line width will be discarded later anyway.
									validate_general_line_width (to_distance (to_string (arg)));
									board_arc.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => null;
					end case;

				-- parent section
				when SEC_GR_CIRCLE =>
					case section.name is
						when SEC_CENTER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_circle.center, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_circle.center, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_circle.point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_circle.point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) = layer_top_silk_screen then
										board_circle.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										board_circle.layer := BOT_SILK;

									elsif to_string (arg) = layer_top_assy_doc then
										board_circle.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										board_circle.layer := BOT_ASSY;
									
									elsif to_string (arg) = layer_top_keepout then
										board_circle.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										board_circle.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_stop_mask then
										board_circle.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										board_circle.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										board_circle.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										board_circle.layer := BOT_PASTE;

									elsif to_string (arg) = layer_edge_cuts then
										board_circle.layer := EDGE_CUTS;
									else
										invalid_layer;
									end if;
								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									-- NOTE: The width of the contour does not matter for the manufacturer.
									-- But for the sake of completeness we check the line width anyway.
									-- The line width will be discarded later anyway.
									validate_general_line_width (to_distance (to_string (arg)));
									board_circle.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => null;
					end case;
					
				-- parent section
				when SEC_GR_LINE =>
					case section.name is
						when SEC_START =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_line.start_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_line.start_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set_point (axis => X, point => board_line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set_point (axis => Y, point => board_line.end_point, value => to_distance (to_string (arg)));
									set_point (axis => Z, point => board_line.end_point, value => zero_distance);
								when others => too_many_arguments;
							end case;

						when SEC_ANGLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS no need to read the angle. start and end point are sufficient.
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									if to_string (arg) = layer_top_silk_screen then
										board_line.layer := TOP_SILK;
									elsif to_string (arg) = layer_bot_silk_screen then
										board_line.layer := BOT_SILK;

									elsif to_string (arg) = layer_top_assy_doc then
										board_line.layer := TOP_ASSY;
									elsif to_string (arg) = layer_bot_assy_doc then
										board_line.layer := BOT_ASSY;
									
									elsif to_string (arg) = layer_top_keepout then
										board_line.layer := TOP_KEEP;
									elsif to_string (arg) = layer_bot_keepout then
										board_line.layer := BOT_KEEP;
									
									elsif to_string (arg) = layer_top_stop_mask then
										board_line.layer := TOP_STOP;
									elsif to_string (arg) = layer_bot_stop_mask then
										board_line.layer := BOT_STOP;
										
									elsif to_string (arg) = layer_top_solder_paste then
										board_line.layer := TOP_PASTE;
									elsif to_string (arg) = layer_bot_solder_paste then
										board_line.layer := BOT_PASTE;

									elsif to_string (arg) = layer_edge_cuts then
										board_line.layer := EDGE_CUTS;
									else
										invalid_layer;
									end if;
								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									-- NOTE: The width of the contour does not matter for the manufacturer.
									-- But for the sake of completeness we check the line width anyway.
									-- The line width will be discarded later anyway.
									validate_general_line_width (to_distance (to_string (arg)));
									board_line.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

							
						when others => null;
					end case;


					
-- 					case section.parent is
-- 						when SEC_MODULE =>
-- 							case section.arg_counter is
-- 								when 0 => null;
-- 								when 1 => null; -- CS read name of 3d model
-- 								when others => too_many_arguments;
-- 							end case;
-- 						when others => invalid_section;
-- 					end case;
-- 					
-- 				when SEC_ROTATE | SEC_SCALE =>
-- 					case section.parent is
-- 						when SEC_MODULE => null; -- CS currently no direct (non-wrapped) arguments follow
-- 						when others => invalid_section;
-- 					end case;
-- 
-- 				when SEC_XYZ =>
-- 					case section.parent is
-- 						when SEC_AT => null; -- CS
-- 						when SEC_SCALE => null; -- CS
-- 						when SEC_ROTATE => null; -- CS
-- 						when others => invalid_section;
-- 					end case;

				when others => null; -- CS remove 
			end case;
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;

		end read_arg;

		procedure exec_section is
		-- Performs an operation according to the active section and variables that have been
		-- set earlier (when processing the arguments. see procedure read_arg).
		-- Restores the previous section.
			use et_pcb_coordinates;
			use et_libraries;

			procedure invalid_layer_reference is begin
				log_indentation_reset;
				log (message_error & "reference " & to_string (package_reference) & " must be in a silk screen layer !", console => true);
				raise constraint_error;
			end invalid_layer_reference;

			procedure invalid_layer_value is begin
				log (message_warning & "value " & to_string (package_value) & " should be in a fabrication layer !");
			end invalid_layer_value;

			procedure invalid_layer_user is begin
				log_indentation_reset;
				log (message_error & "user text " & to_string (package_text.content) 
					 & " must be in a silk screen or fabrication layer !", console => true);
				raise constraint_error;
			end invalid_layer_user;

			procedure invalid_layer is begin
				log_indentation_reset;
				log (message_error & "invalid layer for this object !", console => true);
				raise constraint_error;
			end invalid_layer;

			procedure warn_on_missing_net is begin
			-- Warns operator if a terminal is not connected to a net.
				if et_schematic.type_net_name.length (terminal_net_name) = 0 then
					log (message_warning & to_string (package_reference) & latin_1.space
						 & to_string (terminal_name) & " not connected with a net !");
				end if;
			end warn_on_missing_net;
			
			procedure insert_package is 
			-- Builds and inserts package in board.
			-- Raises alarm if package already exists in container.
			
				-- This cursor points to the last inserted package:
				package_cursor : et_kicad_pcb.type_packages_board.cursor;

				-- This flag goes true once a package is to be inserted that already exists (by its reference).
				package_inserted : boolean;
			begin -- insert_package
				-- CS warning if package_reference is default_component_reference
				-- CS warning if value is empty ?
			
				case package_appearance is
					when REAL =>
						board.packages.insert (
							position	=> package_cursor,
							inserted	=> package_inserted,
							key			=> package_reference,
							new_item	=> (
								position		=> package_position,
								appearance		=> REAL, -- !!!!!!!
								technology		=> package_technology,
								description		=> package_description,
								time_stamp		=> package_time_stamp,
								time_edit		=> package_time_edit,
								value			=> package_value,
								silk_screen		=> package_silk_screen,
								terminals		=> terminals,
								copper			=> package_copper,
								keepout			=> package_keepout,
								stop_mask		=> package_stop_mask,
								solder_stencil	=> package_stencil,
								route_restrict	=> package_route_restrict, -- always empty
								via_restrict	=> package_via_restrict, -- always empty
								assembly_documentation	=> package_assy_doc,
								pcb_contours		=> package_pcb_contours,
								pcb_contours_plated	=> package_pcb_contours_plated,
								package_contours	=> package_contours
								)
							);
						
					when VIRTUAL =>
						board.packages.insert (
							position	=> package_cursor,
							inserted	=> package_inserted,
							key			=> package_reference,
							new_item	=> (
								position		=> package_position,
								appearance		=> VIRTUAL, --- !!!!!!!!
								technology		=> package_technology,
								description		=> package_description,
								time_stamp		=> package_time_stamp,
								time_edit		=> package_time_edit,
								value			=> package_value,
								silk_screen		=> package_silk_screen,
								terminals		=> terminals,
								copper			=> package_copper,
								keepout			=> package_keepout,
								stop_mask		=> package_stop_mask,
								solder_stencil	=> package_stencil,
								route_restrict	=> package_route_restrict, -- always empty
								via_restrict	=> package_via_restrict, -- always empty
								assembly_documentation	=> package_assy_doc,
								pcb_contours		=> package_pcb_contours,
								pcb_contours_plated	=> package_pcb_contours_plated
								-- a virtual package does not have contours
								)
							);
					
				end case;

				-- abort if package already in board file, otherwise log coordinates and properties
				if package_inserted then

					-- log package coordinates
					log ("package " & to_string (package_reference)
						 & et_pcb.package_position (package_position), -- this is a function that returns package coordinates !
						 log_threshold + 1);
					
					-- CS log package properties (at least reference, value, ...) ?

					-- Once a package has been read completely, some variables
					-- must be reset and lists must be cleared for the next package:

					-- reset description and tags
					package_description := to_package_description ("");
					package_tags := to_package_tags ("");

					-- reset technology and appearance
					package_technology := THT;
					package_appearance := REAL;

					-- reset reference and value
					package_reference := et_schematic.default_component_reference;
					package_value := to_value ("");

					-- delete list of terminals
					terminals.clear;

					-- clear silk screen
					package_silk_screen.top.lines.clear;
					package_silk_screen.top.arcs.clear;
					package_silk_screen.top.circles.clear;
					package_silk_screen.top.texts.clear;

					package_silk_screen.bottom.lines.clear;
					package_silk_screen.bottom.arcs.clear;
					package_silk_screen.bottom.circles.clear;
					package_silk_screen.bottom.texts.clear;

					-- clear assembly documentation
					package_assy_doc.top.lines.clear;
					package_assy_doc.top.arcs.clear;
					package_assy_doc.top.circles.clear;
					package_assy_doc.top.texts.clear;

					package_assy_doc.bottom.lines.clear;
					package_assy_doc.bottom.arcs.clear;
					package_assy_doc.bottom.circles.clear;
					package_assy_doc.bottom.texts.clear;

					-- clear keepout
					package_keepout.top.lines.clear;
					package_keepout.top.arcs.clear;
					package_keepout.top.circles.clear;
					-- CS package_top_keepout.texts.clear;

					package_keepout.bottom.lines.clear;
					package_keepout.bottom.arcs.clear;
					package_keepout.bottom.circles.clear;
					-- CS package_bot_keepout.texts.clear;

					-- clear copper
					package_copper.top.lines.clear;
					package_copper.top.arcs.clear;
					package_copper.top.circles.clear;
					package_copper.top.texts.clear;

					package_copper.bottom.lines.clear;
					package_copper.bottom.arcs.clear;
					package_copper.bottom.circles.clear;
					package_copper.bottom.texts.clear;

				else
					log_indentation_reset;
					log (message_error & "package " & to_string (package_reference) 
						& et_pcb.package_position (package_position)
						& " already used !",
						 console => true);
					raise constraint_error;
				end if;
						
			end insert_package;

			procedure insert_layer is
			-- Inserts the layer (when reading section "layers") in the temporarily container "layers".
				layer_cursor : et_kicad_pcb.type_layers.cursor; -- mandatory, never read
				layer_inserted : boolean; -- goes true if layer id already used
			begin -- insert_layer

				-- insert in container "layers"
				board.layers.insert (
					new_item	=> layer,		-- components set in procedure read_arg
					key			=> layer_id,	-- set in procedure read_section
					inserted	=> layer_inserted,
					position	=> layer_cursor);

				-- Abort if layer already in use. The criteria is the layer id.
				if layer_inserted then
					log ("layer id" & type_layer_id'image (layer_id) 
						 & " name " & type_layer_name.to_string (layer.name)
						 & " meaning " & type_layer_meaning'image (layer.meaning), log_threshold + 2);
				else
					log_indentation_reset;
					log (message_error & "layer id" & type_layer_id'image (layer_id) & " already used !", 
						 console => true);
					raise constraint_error;
				end if;
					
			end insert_layer;

			procedure insert_net_class is
			-- Inserts the net class in board
				net_class_inserted	: boolean := false;
				net_class_cursor	: type_net_classes.cursor;
			begin -- insert_net_class
				-- calculate validate restring for regular and micro vias
				net_class_via_restring := (net_class_via_diameter - net_class.via_drill_min) / 2;
				validate_restring_width (net_class_via_restring);
				net_class.via_restring_min := net_class_via_restring;

				net_class_via_restring := (net_class_micro_via_diameter - net_class.micro_via_drill_min) / 2;
				validate_restring_width (net_class_via_restring);
				net_class.micro_via_restring_min := net_class_via_restring;

				board.net_classes.insert (
					key			=> net_class_name,
					new_item 	=> net_class,
					position	=> net_class_cursor,
					inserted	=> net_class_inserted
					);

				if net_class_inserted then
					-- CS log net class properties more detailled
					log ("net class " & to_string (net_class_name), log_threshold + 1);
					
					-- Clean up list of net names for next net class.
					-- CS: We assume, all other components of net_class are provided in 
					-- next net class section and thus become overwritten.
					net_class.net_names.clear;
				else
					log_indentation_reset;
					log (message_error & "net class '" & to_string (net_class_name) & "' already defined !", console => true);
					raise constraint_error;
				end if;
			end insert_net_class;

			procedure insert_net is
			-- Inserts the net in the board
				net_inserted	: boolean := false;
				net_cursor		: type_netlist.cursor;
			begin

				type_netlist.insert (
					container	=> board.netlist,
					new_item	=> netlist_net,
					position	=> net_cursor,
					inserted	=> net_inserted);

				if net_inserted then
					-- log the net id and name. but skip the first dummy net with id 0
					if netlist_net.id > type_net_id'first then
						log ("net id" & type_net_id'image (netlist_net.id) & " name " 
							& et_schematic.to_string (netlist_net.name),
							log_threshold + 1);
					end if;
				else
					log_indentation_reset;
					log (message_error & "either net id" & type_net_id'image (netlist_net.id) 
						& " or net name '" & et_schematic.to_string (netlist_net.name) & "' already used !",
						 console => true);
					raise constraint_error;
				end if;
					
			end insert_net;

			procedure insert_board_arc is begin
				-- Compute the arc end point from its center, start point and angle.
				-- Later the angle is discarded.
				board_arc.end_point := et_pcb_math.arc_end_point (
					board_arc.center, board_arc.start_point, board_arc.angle);

				-- The board_arc is converted back to its anchestor and
				-- depending on the layer extended with specific properties.
				case board_arc.layer is
					when TOP_SILK =>
						board.silk_screen.top.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_silk_screen_properties (TOP, board.silk_screen.top.arcs.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_silk_screen_properties (BOTTOM, board.silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_assy_doc_properties (TOP, board.assy_doc.top.arcs.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_assy_doc_properties (BOTTOM, board.assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						board.stencil.top.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_stencil_properties (TOP, board.stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						board.stencil.bottom.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_stencil_properties (BOTTOM, board.stencil.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						board.stop_mask.top.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_stop_mask_properties (TOP, board.stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.arcs.append ((et_pcb.type_arc (board_arc) with board_arc.width));
						arc_stop_mask_properties (BOTTOM, board.stop_mask.bottom.arcs.last, log_threshold + 1);


					when TOP_KEEP =>
						board.keepout.top.arcs.append ((
							center 		=> board_arc.center, 
							start_point	=> board_arc.start_point,
							end_point	=> board_arc.end_point));  -- line width discarded because this is keepout
						arc_keepout_properties (TOP, board.keepout.top.arcs.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.arcs.append ((
							center 		=> board_arc.center, 
							start_point	=> board_arc.start_point,
							end_point	=> board_arc.end_point)); -- line width discarded because this is keepout
						arc_keepout_properties (BOTTOM, board.keepout.bottom.arcs.last, log_threshold + 1);
						
					when EDGE_CUTS =>
						board.contour.arcs.append ((et_pcb.type_arc (board_arc) with locked => NO));
						arc_pcb_contour_properties (board.contour.arcs.last, log_threshold + 1);
						
					when others => invalid_layer;
				end case;
			end insert_board_arc;

			procedure insert_board_circle is begin
				-- Compute the circle radius from its center and point at circle.
				-- Later the angle is discarded.
				board_circle.radius := et_pcb_math.distance (board_circle.center, board_circle.point);

				-- The board_circle is converted back to its anchestor and
				-- depending on the layer extended with specific properties.
				case board_circle.layer is
					when TOP_SILK =>
						board.silk_screen.top.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_silk_screen_properties (TOP, board.silk_screen.top.circles.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_silk_screen_properties (BOTTOM, board.silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_assy_doc_properties (TOP, board.assy_doc.top.circles.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_assy_doc_properties (BOTTOM, board.assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						board.stencil.top.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_stencil_properties (TOP, board.stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						board.stencil.bottom.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_stencil_properties (BOTTOM, board.stencil.bottom.circles.last, log_threshold + 1);
						

					when TOP_STOP =>
						board.stop_mask.top.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_stop_mask_properties (TOP, board.stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.circles.append ((et_pcb.type_circle (board_circle) with board_circle.width));
						circle_stop_mask_properties (BOTTOM, board.stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						board.keepout.top.circles.append ((
							center	=> board_circle.center,
							radius	=> board_circle.radius));
						circle_keepout_properties (TOP, board.keepout.top.circles.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.circles.append ((
							center	=> board_circle.center,
							radius	=> board_circle.radius));
						circle_keepout_properties (BOTTOM, board.keepout.bottom.circles.last, log_threshold + 1);

						
					when EDGE_CUTS =>
						board.contour.circles.append ((et_pcb.type_circle (board_circle) with locked => NO));
						-- CS log
						
					when others => invalid_layer;
				end case;

			end insert_board_circle;

			procedure insert_board_line is begin
				-- The board_line is converted back to its anchestor and
				-- depending on the layer extended with specific properties.
				case board_line.layer is

					when TOP_SILK =>
						board.silk_screen.top.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_silk_screen_properties (TOP, board.silk_screen.top.lines.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_silk_screen_properties (BOTTOM, board.silk_screen.bottom.lines.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_assy_doc_properties (TOP, board.assy_doc.top.lines.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_assy_doc_properties (BOTTOM, board.assy_doc.bottom.lines.last, log_threshold + 1);


					when TOP_PASTE =>
						board.stencil.top.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_stencil_properties (TOP, board.stencil.top.lines.last, log_threshold + 1);

					when BOT_PASTE =>
						board.stencil.bottom.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_stencil_properties (BOTTOM, board.stencil.bottom.lines.last, log_threshold + 1);

						
					when TOP_STOP =>
						board.stop_mask.top.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_stop_mask_properties (TOP, board.stop_mask.top.lines.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.lines.append ((et_pcb.type_line (board_line) with board_line.width));
						line_stop_mask_properties (BOTTOM, board.stop_mask.bottom.lines.last, log_threshold + 1);


					when TOP_KEEP =>
						board.keepout.top.lines.append ((
							start_point	=> board_line.start_point,
							end_point	=> board_line.end_point ));
						line_keepout_properties (TOP, board.keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.lines.append ((
							start_point	=> board_line.start_point,
							end_point	=> board_line.end_point ));
						line_keepout_properties (BOTTOM, board.keepout.bottom.lines.last, log_threshold + 1);

						
					when EDGE_CUTS =>
						board.contour.lines.append ((et_pcb.type_line (board_line) with locked => NO));
						-- CS log

					when others => invalid_layer;
				end case;
					
			end insert_board_line;

			procedure insert_fp_arc is begin
			-- Append the arc to the container corresponding to the layer. Then log the arc properties.

				-- compute end point of arc from center, start_point and angle
				package_arc.end_point := 
					et_pcb_math.arc_end_point (package_arc.center, package_arc.start_point, package_arc.angle);

				-- The angle of the arc and its layer are now discarded
				-- as the package_arc is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_arc
				-- is formed and appended to the list of silk screen circles.
				case package_arc.layer is
					when TOP_SILK =>
						package_silk_screen.top.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_silk_screen_properties (TOP, package_silk_screen.top.arcs.last, log_threshold + 1);
						
					when BOT_SILK =>
						package_silk_screen.bottom.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_silk_screen_properties (BOTTOM, package_silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						package_assy_doc.top.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_assy_doc_properties (TOP, package_assy_doc.top.arcs.last, log_threshold + 1);
						
					when BOT_ASSY =>
						package_assy_doc.bottom.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_assy_doc_properties (BOTTOM, package_assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_KEEP =>
						package_keepout.top.arcs.append ((
							center 		=> package_arc.center,
							start_point	=> package_arc.start_point, 
							end_point	=> package_arc.end_point)); -- line width discarded because this is keepout
						arc_keepout_properties (TOP, package_keepout.top.arcs.last, log_threshold + 1);
						
					when BOT_KEEP =>
						package_keepout.bottom.arcs.append ((
							center 		=> package_arc.center,
							start_point	=> package_arc.start_point, 
							end_point	=> package_arc.end_point));  -- line width discarded because this is keepout
						arc_keepout_properties (BOTTOM, package_keepout.bottom.arcs.last, log_threshold + 1);

						
					when TOP_COPPER => 
						package_copper.top.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_copper_properties (TOP, package_copper.top.arcs.last, log_threshold + 1);

					when BOT_COPPER => 
						package_copper.bottom.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_copper_properties (BOTTOM, package_copper.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						package_stop_mask.top.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_stop_mask_properties (TOP, package_stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						package_stop_mask.bottom.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_stop_mask_properties (BOTTOM, package_stop_mask.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						package_stencil.top.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_stencil_properties (TOP, package_stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						package_stencil.bottom.arcs.append ((et_pcb.type_arc (package_arc) with package_arc.width));
						arc_stencil_properties (BOTTOM, package_stencil.bottom.arcs.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_arc;
			
			procedure insert_fp_circle is begin
			-- Append the circle to the container corresponding to the layer. Then log the circle properties.

				-- Compute the circle radius from its center and point at circle:
				package_circle.radius := 
					et_pcb_math.distance (package_circle.center, package_circle.point);

				-- The point at the circle and its layer are now discarded
				-- as the package_circle is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_circle
				-- is formed and appended to the list of silk screen circles.
				case package_circle.layer is
					when TOP_SILK =>
						package_silk_screen.top.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_silk_screen_properties (TOP, package_silk_screen.top.circles.last, log_threshold + 1);
						
					when BOT_SILK =>
						package_silk_screen.bottom.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_silk_screen_properties (BOTTOM, package_silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						package_assy_doc.top.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_assy_doc_properties (TOP, package_assy_doc.top.circles.last, log_threshold + 1);
						
					when BOT_ASSY =>
						package_assy_doc.bottom.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_assy_doc_properties (BOTTOM, package_assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						package_keepout.top.circles.append ((
							center 		=> package_circle.center,
							radius		=> package_circle.radius ));  -- line width discarded because this is keepout
						circle_keepout_properties (TOP, package_keepout.top.circles.last, log_threshold + 1);
						
					when BOT_KEEP =>
						package_keepout.bottom.circles.append ((
							center 		=> package_circle.center,
							radius		=> package_circle.radius ));  -- line width discarded because this is keepout
						circle_keepout_properties (BOTTOM, package_keepout.bottom.circles.last, log_threshold + 1);

						
					when TOP_COPPER => 
						package_copper.top.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_copper_properties (TOP, package_copper.top.circles.last, log_threshold + 1);

					when BOT_COPPER => 
						package_copper.bottom.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_copper_properties (BOTTOM, package_copper.bottom.circles.last, log_threshold + 1);

						
					when TOP_STOP =>
						package_stop_mask.top.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_stop_mask_properties (TOP, package_stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						package_stop_mask.bottom.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_stop_mask_properties (BOTTOM, package_stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						package_stencil.top.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_stencil_properties (TOP, package_stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						package_stencil.bottom.circles.append ((et_pcb.type_circle (package_circle) with package_circle.width));
						circle_stencil_properties (BOTTOM, package_stencil.bottom.circles.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_circle;

			procedure insert_fp_line is begin
			-- Append the line to the container corresponding to the layer. Then log the line properties.
				case package_line.layer is
					when TOP_SILK =>
						package_silk_screen.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_silk_screen_properties (TOP, package_silk_screen.top.lines.last, log_threshold + 1);

					when BOT_SILK =>
						package_silk_screen.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_silk_screen_properties (BOTTOM, package_silk_screen.bottom.lines.last, log_threshold + 1);

						
					when TOP_ASSY =>
						package_assy_doc.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_assy_doc_properties (TOP, package_assy_doc.top.lines.last, log_threshold + 1);

					when BOT_ASSY =>
						package_assy_doc.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_assy_doc_properties (BOTTOM, package_assy_doc.bottom.lines.last, log_threshold + 1);

						
					when TOP_KEEP =>
						package_keepout.top.lines.append ((package_line.start_point, package_line.end_point)); -- line width discarded because this is keepout
						line_keepout_properties (TOP, package_keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						package_keepout.bottom.lines.append ((package_line.start_point, package_line.end_point)); -- line width discarded because this is keepout
						line_keepout_properties (BOTTOM, package_keepout.bottom.lines.last, log_threshold + 1);

						
					when TOP_COPPER => 
						package_copper.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_copper_properties (TOP, package_copper.top.lines.last, log_threshold + 1);

					when BOT_COPPER => 
						package_copper.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_copper_properties (BOTTOM, package_copper.bottom.lines.last, log_threshold + 1);

						
					when TOP_STOP =>
						package_stop_mask.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_stop_mask_properties (TOP, package_stop_mask.top.lines.last, log_threshold + 1);

					when BOT_STOP =>
						package_stop_mask.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_stop_mask_properties (BOTTOM, package_stop_mask.bottom.lines.last, log_threshold + 1);

						
					when TOP_PASTE =>
						package_stencil.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_stencil_properties (TOP, package_stencil.top.lines.last, log_threshold + 1);

					when BOT_PASTE =>
						package_stencil.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_stencil_properties (BOTTOM, package_stencil.bottom.lines.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_line;

			procedure insert_terminal is 
			-- Insert a terminal in the list "terminals":
			
				-- This cursor points to the last inserted terminal:
				terminal_cursor : et_kicad_pcb.type_terminals.cursor;
				-- This flag goes true once a terminal is to be inserted that already exists (by its name).
				terminal_inserted : boolean;
			begin -- insert_terminal

				case terminal_technology is
					when THT =>

						if terminal_shape_tht = CIRCULAR then
							terminals.insert (
								key 		=> terminal_name,
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> THT,
												shape 			=> CIRCULAR,
												tht_hole		=> DRILLED,
												width_inner_layers => terminal_copper_width_inner_layers,
												drill_size_cir	=> terminal_drill_size,
												offset_x		=> terminal_drill_offset_x,
												offset_y		=> terminal_drill_offset_y,
												shape_tht		=> terminal_shape_tht,
												position		=> terminal_position,

												-- the pad is connected with a certain net
												net_name		=> terminal_net_name
												));
						else -- NON_CIRCULAR
							case terminal_drill_shape is
								when CIRCULAR =>
									terminals.insert (
										key 		=> terminal_name,
										position	=> terminal_cursor,
										inserted	=> terminal_inserted,
										new_item 	=> (
														technology 		=> THT,
														shape			=> NON_CIRCULAR,
														tht_hole		=> DRILLED,
														width_inner_layers => terminal_copper_width_inner_layers,
														drill_size_dri	=> terminal_drill_size,
														shape_tht		=> terminal_shape_tht,
														position		=> terminal_position,
														size_tht_x		=> terminal_size_x,
														size_tht_y		=> terminal_size_y,

														-- the pad is connected with a certain net
														net_name		=> terminal_net_name
													));

								when SLOTTED =>
									terminals.insert (
										key 		=> terminal_name,
										position	=> terminal_cursor,
										inserted	=> terminal_inserted,
										new_item 	=> (
														technology 		=> THT,
														shape			=> NON_CIRCULAR,
														tht_hole		=> MILLED,
														width_inner_layers => terminal_copper_width_inner_layers,

														-- The plated millings of the hole is a list of lines.
														-- KiCad does not allow arcs or circles for plated millings.
														millings		=> (lines => contour_milled_rectangle_of_pad
																			(center => terminal_position,
																			size_x => terminal_milling_size_x,
																			size_y => terminal_milling_size_y,
																			offset_x => terminal_drill_offset_x,
																			offset_y => terminal_drill_offset_y),

																			arcs => type_pcb_contour_arcs.empty_list,
																			circles => type_pcb_contour_circles.empty_list),
														shape_tht		=> terminal_shape_tht,
														position		=> terminal_position,
														size_tht_x		=> terminal_size_x,
														size_tht_y		=> terminal_size_y,

														-- the pad is connected with a certain net
														net_name		=> terminal_net_name
													));
							end case;

						end if;

						-- reset drill offset
						terminal_drill_offset_x := pad_drill_offset_min; -- in case the next terminal drill has no offset
						terminal_drill_offset_y := pad_drill_offset_min; -- in case the next terminal drill has no offset
						
					when SMT =>

						-- From the SMT terminal face, validate the status of stop mask and solder paste.
						set_stop_and_mask;
						
						if terminal_shape_smt = CIRCULAR then
							terminals.insert (
								key 		=> terminal_name, 
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> SMT,
												shape			=> CIRCULAR,
												tht_hole		=> DRILLED, -- has no meaning here
												shape_smt		=> terminal_shape_smt,
												position		=> terminal_position,

												-- the pad is connected with a certain net
												net_name		=> terminal_net_name,
												
												face 			=> terminal_face,
												stop_mask		=> terminal_stop_mask,
												solder_paste	=> terminal_solder_paste
											));
						else
							terminals.insert (
								key 		=> terminal_name, 
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
												technology 		=> SMT,
												shape			=> NON_CIRCULAR,
												tht_hole		=> DRILLED, -- has no meaning here
												shape_smt		=> terminal_shape_smt,
												position		=> terminal_position,
												
												-- the pad is connected with a certain net
												net_name		=> terminal_net_name,
												
												face 			=> terminal_face,
												stop_mask		=> terminal_stop_mask,
												solder_paste	=> terminal_solder_paste,
												size_smt_x		=> terminal_size_x,
												size_smt_y		=> terminal_size_y
											));
						end if;

						init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)
				end case;

				-- Log terminal properties and reset net name if terminal could be inserted.
				-- Otherwise abort due to a duplicated usage:
				if terminal_inserted then
					et_pcb.terminal_properties (
						terminal		=> et_pcb.type_terminal (et_kicad_pcb.type_terminals.element (terminal_cursor)),
						name			=> et_kicad_pcb.type_terminals.key (terminal_cursor),
						log_threshold	=> log_threshold + 1);

					-- Whether the terminal is connected with a net or not, can be followed by the lenght of
					-- the terminal_net_name. If the terminal (pad) has no net name provided (section SEC_PAD)
					-- the terminal_net_name is empty.
					log_indentation_up;
					if et_schematic.type_net_name.length (terminal_net_name) > 0 then
						log ("connected with net " & et_schematic.to_string (terminal_net_name),
							log_threshold + 1);
					else
						log ("not connected", log_threshold + 1);
					end if;
					log_indentation_down;

					-- reset net name 
					init_terminal_net_name; -- in case the next terminal has no net connected

				else -- terminal could not be inserted
					log_indentation_reset;
					log (message_error & "duplicated terminal " & to_string (terminal_name) & " !", console => true);
					raise constraint_error;
				end if;
					
			end insert_terminal;

			procedure insert_fp_text is begin
			
				-- Since there is no alignment information provided, use default values:
				package_text.alignment := (horizontal => CENTER, vertical => BOTTOM);

				case package_text.meaning is
					when REFERENCE =>

						-- Insert the reference text in the list of texts of silk screen.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). The content of package_text
						-- is passed separately (via "with" statement).
						case package_text.layer is
							when TOP_SILK =>
								package_silk_screen.top.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_silk_screen_properties (TOP, package_silk_screen.top.texts.last, log_threshold + 1);
								
							when BOT_SILK =>
								package_silk_screen.bottom.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_silk_screen_properties (BOTTOM, package_silk_screen.bottom.texts.last, log_threshold + 1);
							when others => -- should never happen
								invalid_layer_reference; 
						end case;

					when VALUE =>

						-- Insert the value text in the list of texts of silk screen.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). The content of package_text
						-- is passed separately (via "with" statement).
						case package_text.layer is
							when TOP_ASSY =>
								package_assy_doc.top.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_assy_doc_properties (TOP, package_assy_doc.top.texts.last, log_threshold + 1);
								
							when BOT_ASSY =>
								package_assy_doc.bottom.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_assy_doc_properties (BOTTOM, package_assy_doc.bottom.texts.last, log_threshold + 1);
								
							when others => -- should never happen
								invalid_layer_value;
						end case;

					when USER =>

						-- Insert the value text in the list of texts of silk screen.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). The content of package_text
						-- is passed separately (via "with" statement).
						-- User specific texts may be placed in both silk screen or assembly documentation.
						case package_text.layer is
							when TOP_SILK => 
								package_silk_screen.top.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_silk_screen_properties (TOP, package_silk_screen.top.texts.last, log_threshold + 1);
								
							when BOT_SILK => 
								package_silk_screen.bottom.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_silk_screen_properties (BOTTOM, package_silk_screen.bottom.texts.last, log_threshold + 1);
								
							when TOP_ASSY => 
								package_assy_doc.top.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_assy_doc_properties (TOP, package_assy_doc.top.texts.last, log_threshold + 1);
								
							when BOT_ASSY => 
								package_assy_doc.bottom.texts.append (
									(et_pcb.type_text (package_text) with content => package_text.content));
								text_assy_doc_properties (BOTTOM, package_assy_doc.bottom.texts.last, log_threshold + 1);
								
							when others -- should never happen. kicad does not allow texts in signal layers 
								=> invalid_layer_user;
						end case;
				end case;
		
			end insert_fp_text;
			
		begin -- exec_section
			log (process_section (section.name), log_threshold + 4);
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION =>
							log (et_kicad.system_name & " version " & pcb_file_format_version_4, log_threshold + 1); 

						when SEC_HOST =>
							log ("host " & host_name_pcbnew & " version " & pcb_new_version_4_0_7, log_threshold + 1);

						when SEC_GENERAL =>
							null; -- CS log general information

						when SEC_PAGE =>
							log ("paper size " & et_general.to_string (board.paper_size), log_threshold + 1);

						when SEC_LAYERS =>
							null; -- nothing to do. work already done on leaving SEC_LAYER_ID

						when SEC_SETUP =>
							null; -- CS board log setup (DRC stuff)

						when SEC_NET =>
							insert_net;
							
						when SEC_NET_CLASS =>
							insert_net_class;

						when SEC_MODULE =>
							insert_package; -- in temporarily container "packages"

						when SEC_GR_ARC =>
							insert_board_arc;

						when SEC_GR_CIRCLE =>
							insert_board_circle;

						when SEC_GR_LINE =>
							insert_board_line;
							
						when others => null;
					end case;

				-- parent section
				when SEC_LAYERS =>
					case section.name is
						when SEC_LAYER_ID =>
							insert_layer; -- in temporarily container "layers"

						when others => null;
					end case;

				-- parent section
				when SEC_SETUP =>
					case section.name is
						when SEC_PCBPLOTPARAMS =>
							null; -- CS log plot parameters
						
						when others => null; -- CS
					end case;
					
				-- parent section
				when SEC_MODULE =>
					case section.name is
						when SEC_TEDIT =>
							log ("time edit  " & string (package_time_edit), log_threshold + 1);

						when SEC_TSTAMP =>
							log ("time stamp " & string (package_time_stamp), log_threshold + 1);
							
						when SEC_DESCR =>
							log (to_string (package_description), log_threshold + 1);
							
						when SEC_TAGS =>
							log (to_string (package_tags), log_threshold + 1);

						when SEC_FP_TEXT =>
							insert_fp_text;
		
						when SEC_FP_LINE =>
							insert_fp_line;

						when SEC_FP_ARC =>
							insert_fp_arc;
		
						when SEC_FP_CIRCLE =>
							insert_fp_circle;
							
						when SEC_PAD =>
							insert_terminal;
						
						when others => null;
					end case;
						
				when others => null;
			end case;

			-- restore previous section from stack
			section := sections_stack.pop;
			log (return_to_section (section.name), log_threshold + 3);
			
			exception
				when event:
					others =>
						log_indentation_reset;
						log (message_error & "in " & file_name, console => true);
						log (message_error & affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (ada.exceptions.exception_message (event));
						raise;
			
		end exec_section;
		


		
		
	begin -- to_board
		log ("parsing/building board ...", log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log ("line " & to_string (current_line), log_threshold + 4);

		-- get position of first opening bracket
		character_cursor := type_current_line.index (current_line, 1 * opening_bracket);

		init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)
		init_terminal_net_name; -- in case the next terminal has no net connected
		
		-- This is the central loop where decisions are made whether to read a section name,
		-- an argument or whether to "execute" a section.
		-- An opening bracket indicates a new (sub)section. A closing bracket indicates that a section
		-- finishes and is to be executed. The loop comes to an end if the sections stack depth 
		-- reaches zero.
		loop
			-- read (sub)section
			<<label_read_section>>
				next_character; -- set character cursor to next character
				read_section;
				next_character; -- set character cursor to next character

				-- if a new subsection starts, read subsection
				if element (current_line, character_cursor) = opening_bracket then goto label_read_section; end if;

			-- read argument
			<<label_read_argument>>
				read_arg;
				next_character; -- set character cursor to next character
			
				-- Test for cb, opening_bracket or other character after argument:
				case element (current_line, character_cursor) is

					-- If closing bracket after argument, the (sub)section ends
					-- and must be executed:
					when closing_bracket => goto label_execute_section;

					-- If another section at a deeper level follows,
					-- read (sub)section:
					when opening_bracket => goto label_read_section;

					-- In case another argument follows, it must be read:
					when others => goto label_read_argument; 
				end case;

			-- execute section
			<<label_execute_section>>
				exec_section;

				-- After executing the section, check the stack depth.
				-- Exit when zero reached (topmost section has been executed).
				if sections_stack.depth = 0 then exit; end if;
				
				next_character; -- set character cursor to next character

				-- Test for cb, opening_bracket or other character after closed section:
				case element (current_line, character_cursor) is

					-- If closing bracket after closed section,
					-- execute parent section:
					when closing_bracket => goto label_execute_section;

					-- If another section at a deeper level follows,
					-- read subsection:
					when opening_bracket => goto label_read_section;

					-- In case an argument follows, it belongs to the parent
					-- section and is to be read:
					when others => goto label_read_argument; 
				end case;
				
		end loop;

		-- check section name. must be top level section
		if section.name /= INIT then -- should never happen
			log_indentation_reset;
			log (message_error & "in " & file_name, console => true);
			log (message_error & "top level section not closed !", console => true);
			raise constraint_error;
		end if;


		return board;
	end to_board;

	
	procedure read_board (
		file_name 		: in string;
		log_threshold	: in et_string_processing.type_log_level) is

		board_handle : ada.text_io.file_type;
		line : type_fields_of_line; -- a line of the board file

		use et_pcb.type_lines;
		lines : et_pcb.type_lines.list; -- all lines of the board file

		-- Here the board data goes. 
		-- CS: If Kicad supports multi boards some day, this must become a list of boards.
		board : type_board;
		
	begin -- read_board
		log ("reading board file ...", log_threshold);
		log_indentation_up;

		if ada.directories.exists (file_name) then
			open (
				file => board_handle,
				mode => in_file,
				name => file_name); -- pwr_supply.kicad_pcb

			-- read board file
			set_input (board_handle);
			while not end_of_file loop
				-- log (get_line);

				-- Store a single line in variable "line" (see et_string_processing.ads)
				line := et_string_processing.read_line (
						line 			=> get_line,
						test_whole_line	=> false, -- comment marks at begin of line matter
						number 			=> ada.text_io.line (current_input),
						ifs 			=> latin_1.space); -- fields are separated by space

				-- insert line in container "lines"
				if field_count (line) > 0 then -- we skip empty or commented lines
					append (lines, line);
				end if;
					
			end loop;
			close (board_handle);

			-- process the board data stored in "lines"
			board := to_board (file_name, lines, log_threshold + 1);
			
		else
			log ("board file " & file_name & " not available. nothing to do.", log_threshold);
		end if;
		
		log_indentation_down;
	end read_board;
	
end et_kicad_pcb;

-- Soli Deo Gloria
