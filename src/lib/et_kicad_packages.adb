------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD COMPONENT PACKAGES                            --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_conventions;

package body et_kicad_packages is

	function to_assembly_technology (tech : in string) return type_assembly_technology is begin
		if tech = "smd" then return SMT;
		elsif tech = "thru_hole" then return THT;
		else
			log (ERROR, "invalid assembly technology", console => true);
			raise constraint_error;
		end if;
	end to_assembly_technology;
	
	function to_string (shape : in type_pad_shape_tht) return string is begin
		return space & to_lower (type_pad_shape_tht'image (shape));
	end to_string;
	
	function to_string (shape : in type_pad_shape_smt) return string is begin
		return space & to_lower (type_pad_shape_smt'image (shape));
	end to_string;

	function to_pad_shape_tht (shape : in string) return type_pad_shape_tht is begin
		if shape = "rect" then return RECTANGULAR;
		elsif shape = "circle" then return CIRCULAR;
		elsif shape = "oval" then return OVAL;
		else
			log (ERROR, "invalid or not supported shape for a THT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_pad_shape_tht;

	function to_pad_shape_smt (shape : in string) return type_pad_shape_smt is begin
		if shape = "rect" then return RECTANGULAR;
		elsif shape = "oval" then return OVAL;
		elsif shape = "circle" then return CIRCULAR;
		else
			log (ERROR, "invalid or not supported shape for an SMT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_pad_shape_smt;

	function to_pad_shape_circle (
		position	: in type_position;
		diameter	: in type_pad_size;
		offset		: in type_point)	-- the offset of the pad from the center
		return type_pad_outline is

		use et_terminals.pac_shapes;
		use pac_polygon_circles;

		circle : type_polygon_circle;
		shape : type_pad_outline; -- to be returned
	begin
		circle.center := type_point (position);
		circle.radius := diameter / 2.0;
		move_by (circle.center, offset);
		append (shape.segments.circles, circle);
		
		return shape;
	end to_pad_shape_circle;

	function to_pad_shape_rectangle (
	-- Converts the given position and dimensions of a rectangular pad
	-- to a list with four lines (top, bottom, right, left).
	-- CS: rework as in to_pad_shape_oval
		center		: in type_position; -- the pad center position (incl. angle)
		size_x		: in type_pad_size;	-- the size in x of the pad
		size_y		: in type_pad_size;	-- the size in y of the pad
		offset		: in type_point)	-- the offset of the pad from the center
		return type_pad_outline is

		use et_terminals.pac_shapes;
		use pac_polygon_lines;
		use et_pcb_coordinates;
		use pac_geometry_brd;

		shape : type_pad_outline; -- to be returned

		-- The given center of the pad also provides us with the angle of rotation:
		--angle : constant type_angle := get_angle (center);
		angle : constant type_rotation := rot (center);

		-- supportive frequently used values
		xp : constant type_distance := size_x / 2;
		xn : constant type_distance := -(xp);

		yp : constant type_distance := size_y / 2;
		yn : constant type_distance := -(yp);

 		-- supportive corner points:
		p11, p12 : type_point;
		p21, p22 : type_point;

		-- These are the four lines we need for the rectangular pad contour:
		line_1, line_2 : type_polygon_line; -- left line, right line
		line_3, line_4 : type_polygon_line; -- upper line, lower line

	begin -- to_pad_shape_rectangle
		-- set supportive cornert points
		p11 := type_point (set (x => xn, y => yp));
		p12 := type_point (set (x => xn, y => yn));

		p21 := type_point (set (x => xp, y => yp));
		p22 := type_point (set (x => xp, y => yn));

		-- rotate supportive points
		rotate_by (p11, angle);
		rotate_by (p11, angle);

		rotate_by (p21, angle);
		rotate_by (p21, angle);

		-- move supportive points by given offset
		move_by (p11, offset);
		move_by (p12, offset);
		
		move_by (p21, offset);
		move_by (p22, offset);

		-- set left line
		line_1.start_point := p11;
		line_1.end_point := p12;
		line_1.id := 1;

		-- set lower line
		line_4.start_point := p12;
		line_4.end_point := p22;
		line_4.id := 2;
		
		-- set right line
		line_2.start_point := p22;
		line_2.end_point := p21;
		line_2.id := 3;

		-- set upper line
		line_3.start_point := p21;
		line_3.end_point := p11;
		line_3.id := 4;
		
		-- build shape
		shape.segments_total := 4; -- altogther we have 4 segments
		shape.segments.lines.append (line_1);
		shape.segments.lines.append (line_4);
		shape.segments.lines.append (line_2);
		shape.segments.lines.append (line_3);
		
		return shape;
	end to_pad_shape_rectangle;

	function to_pad_shape_oval (
	-- Converts the given position and dimensions of an oval pad
	-- to a list with two vertical lines and two arcs (rotation assumed zero).
		center	: in type_position;	-- the pad center position (incl. angle)
		size_x	: in type_pad_size;	-- the size in x of the pad
		size_y	: in type_pad_size;	-- the size in y of the pad
		offset	: in type_point)			-- the offset of the pad from the center
		return type_pad_outline is

		use et_terminals.pac_shapes;
		use pac_polygon_lines;
		use pac_polygon_arcs;		
		use et_pcb_coordinates;
		use pac_geometry_brd;

		shape : type_pad_outline; -- to be returned

		-- The given center of the pad also provides us with the angle of rotation:
		angle : constant type_rotation := rot (center);
		
		-- supportive frequently used values
		x1p : constant type_distance := size_x / 2;
		x1n : constant type_distance := -(x1p);

		y1p : constant type_distance := size_y / 2;
		y1n : constant type_distance := -(y1p);

		x2p : constant type_distance := x1p - y1p;
		x2n : constant type_distance := -(x2p);

		-- supportive points:
		p11, p12 : type_point; -- start/end point of upper line
		p21, p22 : type_point; -- start/end point of lower line
		p41, p42 : type_point; -- center of left/right arc
		
		-- These are the two lines and the two arcs we need for the oval pad contour:
		line_1, line_2 : type_polygon_line;	-- upper/lower line
		arc_1, arc_2 : type_polygon_arc;	-- left/right arc
		
	begin -- to_pad_shape_oval

		-- set supportive points
		-- upper line
		p11 := type_point (set (x => x2n, y => y1p));
		p12 := type_point (set (x => x2p, y => y1p));

		-- lower line
		p21 := type_point (set (x => x2n, y => y1n));
		p22 := type_point (set (x => x2p, y => y1n));

		-- left arc
		p41 := type_point (set (x => x2n,  y => zero));

		-- right arc
		p42 := type_point (set (x => x2p,  y => zero));
		
		-- rotate supportive points 
		rotate_by (p11, angle);
		rotate_by (p12, angle);
		
		rotate_by (p21, angle);
		rotate_by (p22, angle);
		
		rotate_by (p41, angle);
		rotate_by (p42, angle);		

		-- move supportive points by given offset
		move_by (p11, offset);
		move_by (p12, offset);
		
		move_by (p21, offset);
		move_by (p22, offset);
		
		move_by (p41, offset);
		move_by (p42, offset);		
		
		-- set upper line
		line_1.start_point := p11;
		line_1.end_point := p12;
		line_1.id := 1;

		-- set right arc
		arc_2.start_point := p12;
		arc_2.center := p42;
		arc_2.end_point := p22;
		arc_2.id := 2;
		
		-- set lower line
		line_2.start_point := p22;
		line_2.end_point := p21;
		line_2.id := 3;
		
		-- set left arc
		arc_1.start_point := p21;
		arc_1.center := p41;
		arc_1.end_point := p11;
		arc_1.id := 4;
		
		-- build shape
		shape.segments_total := 4;  -- altogther we have 4 segments
		shape.segments.lines.append (line_1);
		shape.segments.arcs.append (arc_2);
		shape.segments.lines.append (line_2);
		shape.segments.arcs.append (arc_1);
		
		return shape;
	end to_pad_shape_oval;
	
	function to_pad_milling_contour (
	-- Converts the given position and dimensions of a rectangular slotted hole
	-- to a list with four lines (top, bottom, right, left).
		center	: in type_position; -- the terminal position (incl. angle, (z axis ignored))
		size_x	: in type_pad_size;	-- the size in x of the hole
		size_y	: in type_pad_size;	-- the size in y of the hole
		offset	: in type_point)	-- the offset of the pad from the center
		return pac_shapes.pac_polygon_lines.list is

		use et_terminals.pac_shapes;
		use et_pcb_coordinates;
		use pac_geometry_brd;

		lines : pac_polygon_lines.list; -- to be returned

		-- The given center of the pad also provides us with the angle of rotation:
		angle : constant type_rotation := rot (center);
		
		-- supportive frequently used values
		xp : constant type_distance := size_x / 2;
		xn : constant type_distance := -(xp);

		yp : constant type_distance := size_y / 2;
		yn : constant type_distance := -(yp);

 		-- supportive corner points:
		p11, p12 : type_point;
		p21, p22 : type_point;

		-- These are the four lines we need for the rectangular pad contour:
		line_1, line_2 : type_polygon_line; -- left line, right line
		line_3, line_4 : type_polygon_line; -- upper line, lower line

	begin -- to_pad_milling_contour
		-- set supportive cornert points
		p11 := type_point (set (x => xn, y => yp)); -- top left
		p12 := type_point (set (x => xn, y => yn)); -- bottom left

		p21 := type_point (set (x => xp, y => yp)); -- top right
		p22 := type_point (set (x => xp, y => yn)); -- bottom right

		-- rotate supportive points
		rotate_by (p11, angle);
		rotate_by (p12, angle);

		rotate_by (p21, angle);
		rotate_by (p22, angle);

		-- move supportive points by given offset
		move_by (p11, offset);
		move_by (p12, offset);
		
		move_by (p21, offset);
		move_by (p22, offset);

		-- set left line
		line_1.start_point	:= p11;
		line_1.end_point	:= p12;

		-- set lower line
		line_4.start_point	:= p12;
		line_4.end_point	:= p22;

		-- set right line
		line_2.start_point	:= p22;
		line_2.end_point	:= p21;

		-- set upper line
		line_3.start_point	:= p21;
		line_3.end_point	:= p11;

		
		-- Assemble milling contour:

		-- The lines must be given an id because
		-- they will end up in a polygon later.
		-- The lines are appended in counterclockwise direction.
		line_1.id := 1;
		lines.append (line_1); -- left

		line_4.id := 2;
		lines.append (line_4); -- lower

		line_2.id := 3;
		lines.append (line_2); -- right

		line_3.id := 4;
		lines.append (line_3); -- upper
		
		return lines;
	end to_pad_milling_contour;
	
	function to_package_model (
		file_name		: in string; -- S_0201.kicad_mod
		lines			: in pac_lines_of_file.list;
		log_threshold	: in et_string_processing.type_log_level)
		return type_package_library is

		use pac_lines_of_file;
		use et_drills;
		use et_terminals;
		use et_packages;
		use et_pcb_coordinates;
		use et_pcb_coordinates.pac_geometry_brd;

		-- Extract the actual package name (like S_0201) from the given file name:
		package_name : pac_package_name.bounded_string :=
			to_package_name (ada.directories.base_name (file_name)); 

		function path_and_file_name return string is
		-- returns the path and file name. used for error messages.
		begin
			--return "file " & ada.directories.compose (
			--	to_string (library_group), file_name);
			return "file " & file_name;
		end path_and_file_name;
		
		-- This cursor points to the line being processed (in the list of lines given in "lines"):
		line_cursor : pac_lines_of_file.cursor := lines.first;

		opening_bracket : constant character := '(';
		closing_bracket : constant character := ')';

		term_char_seq : constant string (1..2) := space & closing_bracket;
		term_char_set : character_set := to_set (term_char_seq);

		-- the section prefix is a workaround due to GNAT reserved keywords.
		sec_prefix : constant string := "sec_";

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
		terminal_name 			: type_terminal_name.bounded_string;
		terminal_technology		: type_assembly_technology;
		terminal_pad_shape_tht 	: type_pad_shape_tht;
		terminal_pad_shape_smt 	: type_pad_shape_smt;

		terminal_face 				: et_pcb_coordinates.type_face;
		terminal_drill_size			: type_drill_size; 
		terminal_hole_shape			: type_tht_hole_shape; -- for slotted holes
		terminal_milling_size_x		: type_pad_milling_size;  -- CS use a composite instead ?
		terminal_milling_size_y		: type_pad_milling_size; 
		terminal_pad_drill_offset	: pac_geometry_brd.type_point;

		-- The center of an smt pad or the position of the drill of a tht pad:
		terminal_position	: pac_geometry_brd.type_position; 
		
		pad_size_x : type_pad_size;  -- CS use a composite instead ?
		pad_size_y : type_pad_size;

-- 		terminal_copper_width_outer_layers : et_pcb_coordinates.type_distance;
		terminal_copper_width_inner_layers : et_pcb_coordinates.type_distance := 1.0; -- CS load from DRU ?

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_solder_paste, terminal_bot_solder_paste : type_solder_paste_status;

		-- This is the flag for the solder paste status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_solder_paste : type_solder_paste_status;

		-- Temporarily these flags hold the solder paste status of an SMT terminal.
		-- They are initialized by procedure init_terminal_layers and validated by
		-- procedure set_stop_and_mask.
		terminal_top_stop_mask, terminal_bot_stop_mask : type_stop_mask_status;

		-- This is the flag for the stop mask status of an SMT terminal.
		-- Read when finally building a terminal.
		terminal_stop_mask_status : type_stop_mask_status;

		-- Here we collect all kinds of terminals after they have been built.
		terminals : et_terminals.type_terminals.map;



	-- TEXTS
		text : type_text_package;

		-- Temporarily text placeholders for reference and value are required. 
		placeholder : et_packages.type_text_placeholder;


		
		
	-- CONTAINERS 

		-- NON ELECTRIC !!! COPPER OBJECTS (lines, arcs, circles)
		-- NOTE: Does not include texts as kicad does not allow texts in signal layers.
		copper : et_packages.type_copper_both_sides;

		-- STOP MASK OBJECTS
		stop_mask : et_packages.type_stop_mask_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated

		-- SOLDER STENCIL OBJECTS
		stencil : et_packages.type_stencil_both_sides;
		-- CS: mind objects explicitely drawn and such auto generated
	
		-- SILK SCREEN OBJECTS (lines, arcs, circles, texts, text placeholders)
		silk_screen : et_packages.type_silk_screen_both_sides;
	
		-- ASSEMBLY DOC (FAB) OBJECTS (lines, arcs, circles, texts, text placeholders)
		assy_doc : et_packages.type_assembly_documentation_both_sides;

		-- KEEPOUT OBJECTS (lines, arcs, circles)
		keepout : et_packages.type_keepout_both_sides;

		procedure init_stop_and_mask is begin
		-- Resets the temporarily status flags of solder paste and stop mask of an SMT terminal.
		-- Does not affect THT terminals (stop mask always open, solder paste never applied).
			terminal_top_solder_paste := type_solder_paste_status'first;
			terminal_bot_solder_paste := type_solder_paste_status'first;
			terminal_top_stop_mask := type_stop_mask_status'first;
			terminal_bot_stop_mask := type_stop_mask_status'first;
		end init_stop_and_mask;

		procedure set_stop_and_mask is
		-- From the SMT terminal face, validates the status of stop mask and solder paste.
			use et_pcb_coordinates;
			
			procedure invalid is begin
				log (ERROR, "contradicting layers in terminal !", console => true);
				log (text => "face " & to_string (terminal_face), console => true);
				log (text => " solder paste top " & to_string (terminal_top_solder_paste), console => true);
				log (text => " solder paste bot " & to_string (terminal_bot_solder_paste), console => true);
				log (text => " stop mask top    " & to_string (terminal_top_stop_mask), console => true);
				log (text => " stop mask bot    " & to_string (terminal_bot_stop_mask), console => true);
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

					terminal_stop_mask_status := terminal_top_stop_mask;
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

					terminal_stop_mask_status := terminal_bot_stop_mask;
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
			if line_cursor /= pac_lines_of_file.no_element then

				-- Since a single line in container "lines" (where line_cursor points to) is a list 
				-- of strings itself, we convert them first to a fixed string and then to a bounded string.
				current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
				log (text => "line " & to_string (current_line), level => log_threshold + 4);
			else
				-- This should never happen:
				log (ERROR, "in " & path_and_file_name, console => true);
				log (ERROR, "no more lines available !", console => true);
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
				log (ERROR, "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' !", console => true);
				raise constraint_error;
			end invalid_section;
		begin
			-- save previous section on stack
			sections_stack.push (section);

			-- the former active section name becomes the parent section name
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

			log (text => enter_section (section.name), level => log_threshold + 5);

			exception
				when event:
					others =>
						log (ERROR, "in " & path_and_file_name, console => true);
						log (ERROR, affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);

						log (ERROR, "section '" & slice (current_line, character_cursor, end_of_kw) 
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
			use et_text.type_text_content;
			use et_pcb_coordinates;
			use pac_geometry_brd;
		
			arg : type_argument.bounded_string; -- here the argument goes temporarily

			procedure invalid_layer is begin
				log (ERROR, "invalid layer " & to_string (arg), console => true);
				raise constraint_error;
			end invalid_layer;

			procedure too_many_arguments is begin
				log (ERROR, "too many arguments in section " & to_string (section.name) & " !", console => true);
				log (text => "excessive argument reads '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end too_many_arguments;

			procedure invalid_fp_text_keyword is begin
				log (ERROR, "expect keyword '" & keyword_fp_text_reference 
					 & "' or '" & keyword_fp_text_value 
					 & "' or '" & keyword_fp_text_user
					 & "' ! found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_fp_text_keyword;

			procedure invalid_placeholder_reference is begin
				log (ERROR, "expect reference placeholder '" & placeholder_reference & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_reference;

			procedure invalid_placeholder_value is
			begin
				log (ERROR, "expect value placeholder '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_placeholder_value;

			procedure invalid_package_name is
			begin
				log (ERROR, "expect package name '" & to_string (package_name) & "' !"
					 & " found '" & to_string (arg) & "'", console => true);
				raise constraint_error;
			end invalid_package_name;

			procedure invalid_component_assembly_face is
			begin
				log (ERROR, "default assembly face " & et_pcb_coordinates.to_string (BOTTOM) 
					 & " found. Must be " & et_pcb_coordinates.to_string (TOP) & " !", console => true);
				raise constraint_error;
			end invalid_component_assembly_face;

			procedure invalid_attribute is
			begin
				log (ERROR, "invalid attribute !", console => true);
				raise constraint_error;
			end invalid_attribute;

			procedure invalid_section is
			begin
				log (ERROR, "invalid subsection '" & to_string (section.name) 
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
					log (ERROR, affected_line (element (line_cursor))
						& space & latin_1.quotation & " expected");
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
			
			log (text => "arg" & to_string (section.arg_counter) & space & to_string (arg), level => log_threshold + 4);

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
									check_timestamp (time_stamp);
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
							-- CS text.hidden := false; -- "hide" flag is optionally provided as last argument. if not, default to false
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
										-- CS text.hidden := true;
										null;
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
									set (axis => X, point => circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => circle.center, value => to_distance (to_string (arg)));
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
									set (axis => X, point => line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => line.start_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => arc.center, value => to_distance (to_string (arg)));
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
									set (axis => X, point => line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => line.end_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => arc.start_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_CIRCLE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => circle.point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				when SEC_ANGLE =>
					case section.parent is
						when SEC_FP_ARC =>
							case section.arg_counter is
								when 0 => null;
								when 1 => arc.angle := to_rotation (to_string (arg));
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
								when 1 => null; -- CS the text width provided by the first argument is ignored.
								when 2 => text.size := to_distance (to_string (arg)); -- text height becomes text size
								when others => too_many_arguments;
							end case;

						when SEC_PAD =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_pad_size (to_distance (to_string (arg)));
									pad_size_x := to_distance (to_string (arg));
								when 2 => 
									validate_pad_size (to_distance (to_string (arg)));
									pad_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
					
				when SEC_THICKNESS =>
					case section.parent is
						when SEC_FONT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => text.line_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_AT =>
					case section.parent is
						when SEC_PAD =>
							set (terminal_position, pac_geometry_brd.zero_rotation); -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
								when 3 => 
									set (terminal_position, to_rotation (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_TEXT =>
							--text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set (text.position, pac_geometry_brd.zero_rotation);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => text.position, value => to_distance (to_string (arg)));
								when 3 => 
									--text.angle := to_angle (to_string (arg));
									set (text.position, to_rotation (to_string (arg)));
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
									if to_string (arg) = tht_hole_shape_oval then -- (drill oval 1.2 5.5)
										terminal_hole_shape := OVAL;
									else
										terminal_hole_shape := CIRCULAR; -- (drill 2.2)
										validate_drill_size (to_distance (to_string (arg)));
										terminal_drill_size := to_distance (to_string (arg));
									end if;
								when 2 =>
									case terminal_hole_shape is
										when CIRCULAR => too_many_arguments;
										when OVAL => terminal_milling_size_x := to_distance (to_string (arg)); -- 1.2
									end case;
								when 3 =>
									case terminal_hole_shape is
										when CIRCULAR => too_many_arguments;
										when OVAL => terminal_milling_size_y := to_distance (to_string (arg)); -- 5.5
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
								when 1 => set (axis => X, point => terminal_pad_drill_offset, value => to_distance (to_string (arg)));
								when 2 => set (axis => Y, point => terminal_pad_drill_offset, value => to_distance (to_string (arg)));
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

									-- Reset pad-drill offset (in case there is no offset given).
									-- This serves as initialize measure.
									reset (terminal_pad_drill_offset); 

								when 2 =>
									terminal_technology := to_assembly_technology (to_string (arg));
									
								when 3 =>
									case terminal_technology is
										when SMT => terminal_pad_shape_smt := to_pad_shape_smt (to_string (arg));
										when THT => terminal_pad_shape_tht := to_pad_shape_tht (to_string (arg));
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
						log (ERROR, "in " & path_and_file_name, console => true);
						log (ERROR, affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (text => ada.exceptions.exception_message (event));
						raise;

		end read_arg;

		procedure exec_section is
		-- Performs an operation according to the active section and variables that have been
		-- set earlier (when processing the arguments. see procedure read_arg).
		-- Restores the previous section.
			use et_pcb_coordinates;

			procedure invalid_layer is begin
				log (ERROR, "invalid layer for this object !", console => true);
				raise constraint_error;
			end invalid_layer;
		
			procedure invalid_layer_reference is begin
				log (ERROR, "reference placeholder must be in a silk screen layer !", console => true);
				raise constraint_error;
			end invalid_layer_reference;

			procedure invalid_layer_value is begin
				log (WARNING, "value placeholder should be in a fabrication layer !");
			end invalid_layer_value;

			procedure invalid_layer_user is begin
				log (ERROR, "user text must be in a silk screen or fabrication layer !", console => true);
				raise constraint_error;
			end invalid_layer_user;

			procedure insert_fp_arc is 
				use et_packages;
			begin
			-- Append the arc to the container corresponding to the layer. Then log the arc properties.
				
				-- compute end point of arc from center, start_point and angle
				arc.end_point := type_point (arc_end_point (arc.center, arc.start_point, arc.angle));

				-- The angle of the arc and its layer are now discarded
				-- as the arc is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_arc
				-- is formed and appended to the list of silk screen circles.
				case arc.layer is
					when TOP_SILK =>
						silk_screen.top.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_silk_screen_properties (TOP, silk_screen.top.arcs.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_silk_screen_properties (BOTTOM, silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_assy_doc_properties (TOP, assy_doc.top.arcs.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_assy_doc_properties (BOTTOM, assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point,
							direction	=> direction_of_rotation (arc.angle)
							));
						
						arc_keepout_properties (TOP, keepout.top.arcs.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point,
							direction	=> direction_of_rotation (arc.angle)
							));

						arc_keepout_properties (BOTTOM, keepout.bottom.arcs.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_copper_properties (TOP, copper.top.arcs.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_copper_properties (BOTTOM, copper.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						stop_mask.top.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_stop_mask_properties (TOP, stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_stop_mask_properties (BOTTOM, stop_mask.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						stencil.top.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_stencil_properties (TOP, stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						stencil.bottom.arcs.append ((pac_shapes.type_arc (arc) with arc.width));
						arc_stencil_properties (BOTTOM, stencil.bottom.arcs.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_arc;

			procedure insert_fp_circle is 
				use et_packages;
			begin
			-- Append the circle to the container corresponding to the layer. Then log the circle properties.

				
				-- Compute the circle radius from its center and point at circle:
				circle.radius := distance_total (circle.center, circle.point);

				-- The point at the circle and its layer are now discarded
				-- as the circle is converted back to its anchestor
				-- and then optionally extended with the line width of the circumfence. 
				-- Thus a type_fillable_circle or a type_fillable_circle_solid
				-- is formed and appended to the corresponding list of circles.
				-- Filling circles is not supported by kicad -> default to no filling.
				case circle.layer is
					when TOP_SILK =>
						silk_screen.top.circles.append ((pac_shapes.type_circle (circle) with 
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 
						
						circle_silk_screen_properties (TOP, silk_screen.top.circles.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 
						
						circle_silk_screen_properties (BOTTOM, silk_screen.bottom.circles.last, log_threshold + 1);
						
					when TOP_ASSY =>
						assy_doc.top.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 

						circle_assy_doc_properties (TOP, assy_doc.top.circles.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 

						circle_assy_doc_properties (BOTTOM, assy_doc.bottom.circles.last, log_threshold + 1);
						
					when TOP_KEEP =>
						keepout.top.circles.append ((pac_shapes.type_circle (circle) with filled => NO));

						circle_keepout_properties (TOP, keepout.top.circles.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.circles.append ((pac_shapes.type_circle (circle) with filled => NO)); 
						
						circle_keepout_properties (BOTTOM, keepout.bottom.circles.last, log_threshold + 1);
						
					when TOP_COPPER => 
						copper.top.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width));
						
						circle_copper_properties (TOP, copper.top.circles.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width));
						
						circle_copper_properties (BOTTOM, copper.bottom.circles.last, log_threshold + 1);
						
					when TOP_STOP =>
						stop_mask.top.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 

						circle_stop_mask_properties (TOP, stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 
						
						circle_stop_mask_properties (BOTTOM, stop_mask.bottom.circles.last, log_threshold + 1);
						
					when TOP_PASTE =>
						stencil.top.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 

						circle_stencil_properties (TOP, stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						stencil.bottom.circles.append ((pac_shapes.type_circle (circle) with
							filled => NO, fill_style => fill_style_default, border_width => circle.width, others => <>)); 

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
						keepout.top.lines.append ((line.start_point, line.end_point));
						line_keepout_properties (TOP, keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						keepout.bottom.lines.append ((line.start_point, line.end_point));
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
			-- Insert a terminal in the list "terminals".
			-- This is library related stuff.

				use et_terminals;
				
				-- this cursor points to the terminal inserted last
				terminal_cursor : et_terminals.type_terminals.cursor;
				
				-- This flag goes true once a terminal is to be inserted that already exists (by its name).
				terminal_inserted : boolean;

				shape : type_pad_outline;

				procedure insert_tht is begin 
				-- NOTE: The pad shape (stored in shape) now must be assigned to
				-- a therminal with either a circular or an oval hole.
					case terminal_hole_shape is
						when CIRCULAR => -- a circular hole
							
							terminals.insert (
								key 		=> terminal_name,
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
									technology 			=> THT,
									tht_hole			=> DRILLED,
									position			=> terminal_position,
									
									-- The shape is the same on top and on bottom side.									
									pad_shape_tht		=> (top => shape, bottom => shape),

									-- CS: We assume there is no option in kicad to cover
									-- a THT pad with stop laquer.
									stop_mask_status_tht	=> stop_mask_status_default,
									
									-- CS: For the stop mask we assume it is just an expansion of the pad shape.
									-- It should be investigated whether kicad supports other stop mask
									-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
									-- It should also be checked whether kicad supports different contours
									-- of top and bottom side of the pad.
									stop_mask_shape_tht	=> (others => <>),

									width_inner_layers 	=> terminal_copper_width_inner_layers,
									drill_size			=> terminal_drill_size
								));

						when OVAL => -- a milled hole
							declare
								lines : pac_polygon_lines.list := to_pad_milling_contour (
													center	=> terminal_position,
													size_x	=> terminal_milling_size_x,
													size_y	=> terminal_milling_size_y,
													offset	=> terminal_pad_drill_offset);

								use pac_polygon_lines;
								total : type_polygon_segment_id := type_polygon_segment_id (length (lines));
							begin
								terminals.insert (
									key 		=> terminal_name,
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
										technology 			=> THT,
										tht_hole			=> MILLED,
										position			=> terminal_position,

										-- CS: We assume there is no option in kicad to cover
										-- a THT pad with stop laquer.
										stop_mask_status_tht	=> stop_mask_status_default,
										
										-- The shape is the same on top and on bottom side.									
										pad_shape_tht		=> (top => shape, bottom => shape),

										-- CS: For the stop mask we assume it is just an expansion of the pad shape.
										-- It should be investigated whether kicad supports other stop mask
										-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
										-- It should also be checked whether kicad supports different contours
										-- of top and bottom side of the pad.
										stop_mask_shape_tht	=> (others => <>),

										width_inner_layers	=> terminal_copper_width_inner_layers,

										-- The plated millings of the hole is a list of lines.
										millings => (
											segments => (lines, others	=> <>),
											-- KiCad does not allow arcs or circles for plated millings.
											-- So we have only lines and nothing else.
											segments_total => total)
									));
							end;
					end case;
				end insert_tht;
				
			begin -- insert_terminal
	
				case terminal_technology is
					when THT =>

						case terminal_pad_shape_tht is
							when CIRCULAR =>
								-- Caclulate the pad shape. It is a circle. 
								-- Therefore the size in x serves as diameter.
								shape := to_pad_shape_circle (
											terminal_position, pad_size_x, 
											terminal_pad_drill_offset);
								
								terminals.insert (
									key 		=> terminal_name,
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
										technology 			=> THT,
										tht_hole			=> DRILLED,
										position			=> terminal_position,

										-- CS: We assume there is no option in kicad to cover
										-- a THT pad with stop laquer.
										stop_mask_status_tht	=> stop_mask_status_default,
										
										-- The shape is the same on top and on bottom side.									
										pad_shape_tht		=> (top => shape, bottom => shape),

										-- CS: For the stop mask we assume it is just an expansion of the pad shape.
										-- It should be investigated whether kicad supports other stop mask
										-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
										-- It should also be checked whether kicad supports different contours
										-- of top and bottom side of the pad.
										stop_mask_shape_tht	=> (others => <>),

										width_inner_layers	=> terminal_copper_width_inner_layers,
										drill_size			=> terminal_drill_size
										));

							when RECTANGULAR =>
								-- Calculate the pad shape.
								shape := to_pad_shape_rectangle (
											center		=> terminal_position,
											size_x 		=> pad_size_x,
											size_y 		=> pad_size_y,
											offset		=> terminal_pad_drill_offset);

								insert_tht;

							when OVAL => 
								-- Calculate the pad shape.
								shape := to_pad_shape_oval (
											center		=> terminal_position,
											size_x 		=> pad_size_x,
											size_y 		=> pad_size_y,
											offset		=> terminal_pad_drill_offset);

								insert_tht;

						end case;

				
					when SMT =>

						-- From the SMT terminal face, validate the status of stop mask and solder paste.
						set_stop_and_mask;
						
						case terminal_pad_shape_smt is
							when CIRCULAR =>

								-- Caclulate the pad shape. It is a circle. 
								-- Therefore the size in x serves as diameter.
								shape := to_pad_shape_circle (
											terminal_position, pad_size_x, 
											terminal_pad_drill_offset);
								
								terminals.insert (
									key 		=> terminal_name, 
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
										technology 		=> SMT,
										tht_hole		=> DRILLED, -- has no meaning here
										position		=> terminal_position,
										pad_shape_smt	=> shape,

										-- CS: For the stop mask we assume it is just an expansion of the pad shape.
										-- It should be investigated whether kicad supports other stop mask
										-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
										stop_mask_shape_smt => (others => <>),

										face 					=> terminal_face,
										stop_mask_status_smt	=> terminal_stop_mask_status,
										solder_paste_status		=> terminal_solder_paste,

										-- CS: For the stencil shape we assume it is just the same as the pad shape.
										-- It should be investigated whether kicad supports other stencil shapes
										-- types like SHRINK_PAD or USER_SPECIFIC (see et_terminals.type_stencil_shape).
										stencil_shape		=> (others => <>)
										));
							
							when RECTANGULAR =>

								-- Calculate the rectangular pad shape.
								shape := to_pad_shape_rectangle (
											center		=> terminal_position,
											size_x 		=> pad_size_x,
											size_y 		=> pad_size_y,
											offset 		=> terminal_pad_drill_offset);

								terminals.insert (
									key 		=> terminal_name, 
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
										technology 		=> SMT,
										tht_hole		=> DRILLED, -- has no meaning here
										position		=> terminal_position,
										pad_shape_smt	=> shape,

										-- CS: For the stop mask we assume it is just an expansion of the pad shape.
										-- It should be investigated whether kicad supports other stop mask
										-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
										stop_mask_shape_smt => (others => <>),

										face 					=> terminal_face,
										stop_mask_status_smt	=> terminal_stop_mask_status,
										solder_paste_status		=> terminal_solder_paste,

										-- CS: For the stencil shape we assume it is just the same as the pad shape.
										-- It should be investigated whether kicad supports other stencil shapes
										-- types like SHRINK_PAD or USER_SPECIFIC (see et_terminals.type_stencil_shape).
										stencil_shape		=> (others => <>)
										));

								
							when OVAL =>

								-- Calculate the oval pad shape.
								shape := to_pad_shape_oval (
											center		=> terminal_position,
											size_x 		=> pad_size_x,
											size_y 		=> pad_size_y,
											offset		=> terminal_pad_drill_offset);

								terminals.insert (
									key 		=> terminal_name, 
									position	=> terminal_cursor,
									inserted	=> terminal_inserted,
									new_item 	=> (
										technology 		=> SMT,
										tht_hole		=> DRILLED, -- has no meaning here
										position		=> terminal_position,
										pad_shape_smt	=> shape,

										-- CS: For the stop mask we assume it is just an expansion of the pad shape.
										-- It should be investigated whether kicad supports other stop mask
										-- types like AS_PAD or USER_SPECIFIC (see et_terminals.type_stop_mask_shape).
										stop_mask_shape_smt => (others => <>),

										face 					=> terminal_face,
										stop_mask_status_smt	=> terminal_stop_mask_status,
										solder_paste_status		=> terminal_solder_paste,

										-- CS: For the stencil shape we assume it is just the same as the pad shape.
										-- It should be investigated whether kicad supports other stencil shapes
										-- types like SHRINK_PAD or USER_SPECIFIC (see et_terminals.type_stencil_shape).
										stencil_shape		=> (others => <>)
										));
								
						end case;

						init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)
				end case;

				if terminal_inserted then
					et_terminals.terminal_properties (
						terminal		=> et_terminals.type_terminals.element (terminal_cursor),
						name			=> et_terminals.type_terminals.key (terminal_cursor),
						log_threshold	=> log_threshold + 1);
				else
					log (ERROR, "duplicated terminal " & to_string (terminal_name) & " !", console => true);
					raise constraint_error;
				end if;

			end insert_terminal;

			procedure insert_fp_text is 
				use et_packages;
				use et_text;
			begin
					
				-- Since there is no alignment information provided, use default values:
				text.alignment := (horizontal => CENTER, vertical => BOTTOM);

				case text.meaning is
					when REFERENCE =>
						placeholder := (et_packages.type_text (text) with meaning => NAME);
						
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
						placeholder := (et_packages.type_text (text) with meaning => VALUE);
						
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
						-- If there are invalid characters in user text then they will be replaced
						-- by the character defined in et_text.replace_by_default.
						replace_invalid_characters (text.content);
						
						case text.layer is
							when TOP_SILK => 
								silk_screen.top.texts.append ((et_packages.type_text (text) with content => text.content));
								text_silk_screen_properties (TOP, silk_screen.top.texts.last, log_threshold + 1);
								
							when BOT_SILK => 
								silk_screen.bottom.texts.append ((et_packages.type_text (text) with content => text.content));
								text_silk_screen_properties (BOTTOM, silk_screen.bottom.texts.last, log_threshold + 1);
								
							when TOP_ASSY => 
								assy_doc.top.texts.append ((et_packages.type_text (text) with content => text.content));
								text_assy_doc_properties (TOP, assy_doc.top.texts.last, log_threshold + 1);
								
							when BOT_ASSY => 
								assy_doc.bottom.texts.append ((et_packages.type_text (text) with content => text.content));
								text_assy_doc_properties (BOTTOM, assy_doc.bottom.texts.last, log_threshold + 1);
								
							when others -- should never happen. kicad does not allow texts in signal layers 
								=> invalid_layer_user;
						end case;
				end case;
			end insert_fp_text;

			
		begin -- exec_section
			log (text => process_section (section.name), level => log_threshold + 5);

			-- CS case construct for section.parent (as with board packages)
			
			case section.name is

				when SEC_TEDIT =>
					log (text => "timestamp " & string (time_stamp), level => log_threshold + 1);

				when SEC_DESCR =>
					log (text => to_string (description, verbose => true), level => log_threshold + 1);
					
				when SEC_TAGS =>
					log (text => to_string (tags), level => log_threshold + 1);

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
			log (text => return_to_section (section.name), level => log_threshold + 5);
			
			exception
				when event:
					others =>
						log (ERROR, "in " & path_and_file_name, console => true);
						log (ERROR, affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (text => ada.exceptions.exception_message (event));
						raise;
			
		end exec_section;
		
		procedure check_placeholders is
		-- Checks if there is at least one placeholder for reference and for value.
		-- CS: validate text sizes and width according to specifications in configuration file
			use et_pcb_coordinates;
			use pac_text_placeholders;
			cursor 		: pac_text_placeholders.cursor;
			placeholder : type_text_placeholder;
			reference_found, value_found : boolean := false;
		begin
			-- There must be a placeholder for the reference in the top silk screen:
			cursor := silk_screen.top.placeholders.first;
			while cursor /= pac_text_placeholders.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = NAME then
					reference_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not reference_found then
				log (ERROR, "in " & path_and_file_name, console => true);
				log (ERROR, "no placeholder for component " 
					 & to_string (NAME) 
					 & " found in " & to_string (TOP) & " silk screen !", console => true);
				raise constraint_error;
			end if;

			-- There must be a placeholder for the value in the top assembly documentation:
			cursor := assy_doc.top.placeholders.first;
			while cursor /= pac_text_placeholders.no_element loop
				placeholder := element (cursor);
				if placeholder.meaning = VALUE then
					value_found := true;
					exit;
				end if;
				next (cursor);
			end loop;

			if not value_found then
				log (ERROR, "in " & path_and_file_name, console => true);
				log (ERROR, "no placeholder for component " 
					 & to_string (VALUE) 
					 & " found in " & to_string (TOP) & " assembly documentation !", console => true);
				raise constraint_error;
			end if;
			
		end check_placeholders;

		procedure check_technology is
		-- If the package is REAL, counts the tht and smd terminals. 
		-- Warns operator if the package technology
		-- is not set according to the majority of terminals respectively.
			use et_terminals.type_terminals;
			cursor : et_terminals.type_terminals.cursor := terminals.first;
			tht_count, smt_count : natural := 0; -- the number of THT or SMT terminals

			function number (count : in natural) return string is begin
				return " (" & trim (positive'image (count), left) & "). ";
			end number;
		
		begin -- check_technology
			log (text => "checking package technology vs. terminal count ...", level => log_threshold + 1);
			log_indentation_up;
			
			log (text => "appearance " & to_string (package_appearance), level => log_threshold + 1);
			
			if package_appearance = REAL then
				log (text => "assembly technology " & to_string (package_technology), level => log_threshold + 1);
			
				while cursor /= et_terminals.type_terminals.no_element loop
					case element (cursor).technology is
						when THT => tht_count := tht_count + 1;
						when SMT => smt_count := smt_count + 1;
					end case;
					next (cursor);
				end loop;

				case package_technology is
					when THT =>
						if tht_count < smt_count then
							log (WARNING, "in " & path_and_file_name &
								" majority of terminals is " & to_string (SMT) &
								number (smt_count) &
								"Package technology should be " & to_string (SMT) & " !");
						end if;

					when SMT =>
						if smt_count < tht_count then
							log (WARNING, "in " & path_and_file_name &
								" majority of terminals is " & to_string (THT) &
								number (tht_count) &
								"Package technology should be " & to_string (THT) & " !");
						end if;
				end case;

			end if;
			log_indentation_down;
		end check_technology;
		
	begin -- to_package_model
		log (text => "parsing/building model ...", level => log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log (text => "line " & to_string (current_line), level => log_threshold + 4);
		
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
			log (ERROR, "in " & path_and_file_name, console => true);
			log (ERROR, "top level section not closed !", console => true);
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
					--package_contour			=> (others => <>), -- CS to be filled from 3d model
					pcb_contour				=> (others => <>), -- kicad does allow pcb contours in a package

					--pcb_contour_plated 		=> (others => <>), -- kicad does allow plated pcb contours in a package
					-- CS: currently no need
					
					terminals				=> terminals,
					copper					=> copper, -- non electric !
					silk_screen				=> silk_screen,
					keepout					=> keepout,
					stop_mask				=> stop_mask,
					stencil					=> stencil,
					route_restrict 			=> (others => <>), -- kicad does not know route restrict
					via_restrict 			=> (others => <>), -- kicad does not know via restrict
					assembly_documentation 	=> assy_doc,
					time_stamp				=> time_stamp,
					description				=> description,
					technology				=> package_technology
					);

			when VIRTUAL => -- no package_contours
				return (
					appearance				=> VIRTUAL,
					pcb_contour				=> (others => <>), -- kicad does allow pcb contours in a package
					
					--pcb_contour_plated 		=> (others => <>), -- kicad does allow plated pcb contours in a package
					-- CS: currently no need
					
					terminals				=> terminals,
					copper					=> copper, -- non electric !
					silk_screen				=> silk_screen,
					keepout					=> keepout,
					stop_mask				=> stop_mask,
					stencil					=> stencil,
					route_restrict 			=> (others => <>), -- kicad does not know route restrict
					via_restrict 			=> (others => <>), -- kicad does not know via restrict
					assembly_documentation 	=> assy_doc,
					time_stamp				=> time_stamp,
					description				=> description,
					technology				=> package_technology
					);
		end case;
				
	end to_package_model;

	
	procedure read_libraries (
	-- Reads package libraries.
	-- V4: 
	-- 	- Creates the libraries in container package_libraries.
	-- 	- Bases on search_list_project_lib_dirs (created on reading the project file).
	-- V5:
	--	- The list package_libraries has been created on reading the project file with empty libraries inside.
	-- 	- Now the libraries must be filled.
		log_threshold 	: in et_string_processing.type_log_level) is

		use ada.directories;
		use et_general;
		use et_general.pac_directory_entries;
		use et_packages;

		-- V4 RELATED ------------------------------------------------------------------------------------------
		-- The directory search lists have been created on reading the project file.
		-- Set lib_dir_cursor to first directory.
		use type_project_lib_dirs;
		lib_dir_cursor : type_project_lib_dirs.cursor := search_list_project_lib_dirs.first;
	
		-- backup the directory of origin
		use type_directory_name;
		origin_directory : type_directory_name.bounded_string := to_bounded_string (current_directory);
	
		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		library_names : pac_directory_entries.list;
		library_name_cursor : pac_directory_entries.cursor;
		
		-- While inserting the libraries the flag library_inserted goes true once
		-- inserting was successuful. The flag goes false if a library already exist.
		-- This is happens if a library has already been created via the import of another project.
		library_inserted : boolean;
		--------------------------------------------------------------------------------------------------------
		
		-- The library_cursor points to the library in the container package_libraries.
		use type_libraries;
		library_cursor : type_libraries.cursor;
		
		procedure read_packages (
		-- Creates empty packages in the package_libraries. The package names are
		-- named after the packages found in the library directories.
			library_name	: in type_package_library_name.bounded_string;
			packages		: in out type_packages_library.map) is

			package_names : pac_directory_entries.list;
			package_name_cursor : pac_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
			line : type_fields_of_line; -- a line of a package model

			use pac_lines_of_file;
			lines : pac_lines_of_file.list; -- all lines of a single package model

		begin -- read_packages
			log (text => "reading package names in " & current_directory & " ...", level => log_threshold + 3);
			log_indentation_up;

			package_names := directory_entries (
								target_directory	=> current_directory, 
								category			=> ada.directories.ordinary_file,
								pattern				=> package_pattern);

			-- show number of package libraries
			if is_empty (package_names) then
				log (WARNING, "library " & to_string (library_name) & " is empty !");
			else
				log (text => "found" & count_type'image (length (package_names)) & " packages", level => log_threshold + 4);
			end if;
			
			log_indentation_up;

			package_name_cursor := package_names.first;
			while package_name_cursor /= pac_directory_entries.no_element loop
				log (text => element (package_name_cursor), level => log_threshold + 5);
				log_indentation_up;
				
				-- open package model file
				open (
					file => library_handle,
					mode => in_file,
					name => element (package_name_cursor)); -- S_0201.kicad_mod

				-- read lines of model file
				set_input (library_handle);
				while not end_of_file loop

					-- Store a single line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
						line 			=> get_line,
						comment_mark	=> comment_mark,
						number 			=> ada.text_io.line (current_input),
						ifs 			=> space); -- fields are separated by space

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
						log (text => ada.exceptions.exception_message (event), console => true);
						raise;

		end read_packages;


		use et_import;
		
	begin -- read_libraries
		log (text => "reading package libraries ...", level => log_threshold);
		log_indentation_up;

		case cad_format is
			when KICAD_V4 =>
				
				-- loop in search_list_project_lib_dirs and scan for package libraries (*.pretty stuff)
				while lib_dir_cursor /= type_project_lib_dirs.no_element loop

					log (text => "in directory " & to_string (element (lib_dir_cursor)), level => log_threshold + 1);
					
					-- Scan for package library in directory indicated by lib_dir_cursor:
					library_names := directory_entries (
						target_directory	=> to_string (element (lib_dir_cursor)),  
						category			=> ada.directories.directory,
						pattern				=> package_library_pattern); -- *.pretty stuff

					-- If directory contains no packages, notify operator that there are no package libraries.
					-- Otherwise loop through the library names and create the libraries in container package_libraries.
					if is_empty (library_names) then
						log (WARNING, "no package libraries found in " &
							to_string (element (lib_dir_cursor)) & " !");
					else
						-- show number of package libraries found in the directory
						log (text => "found" & count_type'image (length (library_names)) & " libraries", level => log_threshold + 2);
						log_indentation_up;

						-- Loop through library_names and create the same-named empty libraries 
						-- in container package_libraries:
						library_name_cursor := library_names.first;
						while library_name_cursor /= pac_directory_entries.no_element loop
							log (text => "reading " & element (library_name_cursor) & " ...", level => log_threshold + 2);

							-- create the (empty) library in container package_libraries
							type_libraries.insert (
								container	=> package_libraries,
								key			=> to_file_name (compose ( -- ../lbr/tht_packages/plcc.pretty 
										containing_directory	=> to_string (element (lib_dir_cursor)),
										name					=> element (library_name_cursor))),
								inserted	=> library_inserted,
								position	=> library_cursor,
								new_item	=> type_packages_library.empty_map);

							-- If library has been created already (by import of other project) then there
							-- is no need to read it again.
							if library_inserted then
								-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
								set_directory (compose (
									containing_directory	=> to_string (element (lib_dir_cursor)),
									name					=> element (library_name_cursor)));

								-- Read the library contents and store them in package_libraries where
								-- library_cursor is pointing to:
								type_libraries.update_element (
									container	=> package_libraries,
									position	=> library_cursor,
									process		=> read_packages'access);

								-- change back to directory of origin
								set_directory (et_packages.to_string (origin_directory));
							else
								log (text => " already loaded -> skipped", level => log_threshold + 2);
							end if;
							
							next (library_name_cursor);
						end loop;

						log_indentation_down;
					end if;


					next (lib_dir_cursor);
				end loop;


			when KICAD_V5 =>
				-- Fill empty package_libraries.
				-- Loop in list package_libraries and fill one library after another:
				library_cursor := package_libraries.first;
				
				while library_cursor /= type_libraries.no_element loop
					-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
					set_directory (to_string (key (library_cursor)));

					type_libraries.update_element (
						container	=> package_libraries,
						position	=> library_cursor,
						process		=> read_packages'access);

					-- change back to directory of origin
					set_directory (et_packages.to_string (origin_directory));

					next (library_cursor);
				end loop;
				
			when others =>
				raise constraint_error;
				
		end case;
		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					put_line (ada.exceptions.exception_message (event));
					raise;

	end read_libraries;


	
	
end et_kicad_packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
