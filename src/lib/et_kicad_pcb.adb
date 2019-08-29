------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              KICAD PCB                                   --
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

with et_general;				use et_general;
with et_import;
with kicad_coordinates;
with et_libraries;
with et_schematic;
with et_kicad_general;			use et_kicad_general;
with et_pcb;
with et_pcb_coordinates;
with et_string_processing;		use et_string_processing;

with et_kicad;

package body et_kicad_pcb is
	
	use et_pcb_coordinates.geometry;
	
	use et_general.type_net_name;

	function full_library_name (
		library_name	: in type_library_name.bounded_string; -- bel_logic
		package_name 	: in et_libraries.type_component_package_name.bounded_string; -- S_SO14
		log_threshold	: in et_string_processing.type_log_level)
		return type_package_library_name.bounded_string is
	-- Returns the full library name of the library that
	-- contains the given package library with the given package.
		
	-- V4:
	-- 	- Searches the library directories in the order given in search_list_project_lib_dirs.
	-- 	- The full library name is the result of a search operation:
	-- 	- The first directory and the first library that contains the package.
	-- 	- There can be many library directories to search in.

	-- V5:
	--	- Looks up the fp-lib-table for the first occurence of the given library name.
	--	- The entry in the fp-lib-table in turn provides the full library name (incl. path).
		
		lib : type_package_library_name.bounded_string; -- to be returned

		use et_kicad;
		use et_import;
		use type_project_lib_dirs;
		use et_libraries;
		use type_package_library_name;
		use ada.directories;

		-- V4:
		dir_cursor : et_kicad.type_project_lib_dirs.cursor := et_kicad.search_list_project_lib_dirs.first; -- CS access search_list_library_dirs in module instead
		lib_cursor : et_kicad_pcb.type_libraries.cursor;

		-- V5:
		use type_lib_table;
		fp_lib_table_cursor : type_lib_table.cursor := fp_lib_tables.first; -- CS access fp_lib_tables in module.fp_lib_tables instead

		use type_library_name;
		full_library_name : type_package_library_name.bounded_string;
		package_found : boolean := false;

		procedure search_package (
		-- Searches the library (indicated by lib_cursor) for the given package.
		-- Sets the flag package_found if the library contains the given package.
			lib_name	: in type_package_library_name.bounded_string;
			library		: in type_packages_library.map) is
		begin
			if type_packages_library.contains (
				container	=> library,
				key			=> package_name) then

				package_found := true;
			end if;
		end search_package;
	
	begin -- full_library_name
		log (text => "locating library '" & et_kicad_general.to_string (library_name) &
			"' containing package '" & to_string (package_name) & "' ...", level => log_threshold);
		log_indentation_up;

		case cad_format is
			when KICAD_V4 =>
				
				-- Loop in search_list_project_lib_dirs. Test if the given library
				-- exists in the directory indicated by dir_cursor..
				while dir_cursor /= et_kicad.type_project_lib_dirs.no_element loop

					-- Test if library exists. package_libraries hosts libraries by their full name.
					-- So the library to test is formed by the current directory name, the given library name
					-- and the package_library_directory_extension (*.pretty)
					full_library_name := to_file_name (ada.directories.compose (
						containing_directory	=> to_string (element (dir_cursor)),
						name					=> et_kicad_general.to_string (library_name),
						extension				=> package_library_directory_extension (2..package_library_directory_extension'last))); 

					log (text => "searching in " & et_libraries.to_string (full_library_name) & " ...", level => log_threshold + 1);
					
					lib_cursor := et_kicad_pcb.type_libraries.find (
						container	=> package_libraries,
						key			=> full_library_name);

					-- If library exists, lib_cursor will point to it. Then the library can be searched 
					-- for the given package.
					if et_kicad_pcb.type_libraries."/=" (lib_cursor, et_kicad_pcb.type_libraries.no_element) then

						-- search the library for the given package
						et_kicad_pcb.type_libraries.query_element (
							position	=> lib_cursor,
							process		=> search_package'access);

						-- The search ends as soon as the given package was found.
						if package_found then exit; end if;

					end if;
					
					next (dir_cursor);
				end loop;

			when KICAD_V5 =>

				-- Search for the given library_name in the fp-lib-tables.
				-- The first matching entry in the table provides the full library name (uri).
				-- Then search in that library for the given package_name. If the package is 
				-- not in the library, search for next matching entry in fp-lib-table ...
				while fp_lib_table_cursor /= type_lib_table.no_element loop

					-- On match, open the library (by its uri).
					if element (fp_lib_table_cursor).lib_name = library_name then

						full_library_name := to_file_name (to_string (element (fp_lib_table_cursor).lib_uri));

						log (text => "searching in " & et_libraries.to_string (full_library_name) & " ...", level => log_threshold + 1);
						
						-- locate the library by full name (uri)
						lib_cursor := et_kicad_pcb.type_libraries.find (
									container	=> et_kicad_pcb.package_libraries,
									key			=> full_library_name);
						
						-- Search in the library for the given package
						et_kicad_pcb.type_libraries.query_element (
							position	=> lib_cursor,
							process		=> search_package'access);

						-- The search ends as soon as the given package was found.
						if package_found then exit; end if;

					end if;

					next (fp_lib_table_cursor); -- advance to next entry in fp-lib-table
				end loop;

				-- If the library could not be located anywhere, abort here:
				if length (full_library_name) = 0 then
					log (ERROR, "No library '" & et_kicad_general.to_string (library_name) 
						 & "' found ! Check local and global fp-lib-tables !", console => true);
					raise constraint_error;
				end if;

			when others =>
				raise constraint_error;
				
		end case;
				
		if package_found then
			log (text => " found !", level => log_threshold + 2);
		else
			log (ERROR, "package '" & to_string (package_name) &
				"' not found in any library named '" & et_kicad_general.to_string (library_name) & "' !", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;
		
		return full_library_name;
	end full_library_name;
	
	function to_plot_output_directory (directory : in string) return type_plot_output_directory.bounded_string is
	begin
		return type_plot_output_directory.to_bounded_string (directory);
	end to_plot_output_directory;
	
	function to_string (directory : in type_plot_output_directory.bounded_string) return string is
	begin
		return type_plot_output_directory.to_string (directory);
	end to_string;

	function to_net_id (net_id : in string) return type_net_id is
	-- returns the given net id as type_net_id
	begin
		return type_net_id'value (net_id);
	end to_net_id;
	
	function to_string (net_id : in type_net_id) return string is
	-- returns the given net id as string.
	begin
		return type_net_id'image (net_id);
	end to_string;
	
	function right_net_before_left (right, left : in type_netlist_net) return boolean is
	-- Returns true if the right net id comes beforr the left net id AND
	-- if the right net name differs from the left net name.
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
			log (ERROR, "invalid assembly technology", console => true);
			raise constraint_error;
		end if;
	end to_assembly_technology;
			
	function to_pad_shape_tht (shape : in string) return type_pad_shape_tht is
	begin
		if shape = "rect" then return RECTANGULAR;
		elsif shape = "circle" then return CIRCULAR;
		elsif shape = "oval" then return OVAL;
		else
			log (ERROR, "invalid or not supported shape for a THT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_pad_shape_tht;

	function to_pad_shape_smt (shape : in string) return type_pad_shape_smt is
	begin
		if shape = "rect" then return RECTANGULAR;
		elsif shape = "oval" then return OVAL;
		elsif shape = "circle" then return CIRCULAR;
		else
			log (ERROR, "invalid or not supported shape for an SMT terminal !", console => true);
			raise constraint_error;
		end if;
	end to_pad_shape_smt;

	function to_signal_layer_id (layer : in string) return type_signal_layer_id is
	-- Translates a string like F.Cu or In2.Cu or or In15.Cu to a type_signal_layer_id (0..31) -- see spec
		id : type_signal_layer_id; -- to be returned

		procedure invalid_layer is begin
			log (ERROR, "invalid layer '" & layer & "' !", console => true);
			raise constraint_error;
		end invalid_layer;

	begin
		-- If the given layer is top or bottom (0 or 31):
		-- The bottom layer in kicad is always number 31. Top layer is always number 0.
		if layer = layer_top_copper then id := type_signal_layer_id'first; 
		elsif layer = layer_bot_copper then id := type_signal_layer_id'last;

		-- If the given layer is an inner signal layer:
		-- Check for the layer_inner_prefix ("In") on the very left:
		elsif layer (layer'first .. layer'first - 1 + layer_inner_prefix'last) = layer_inner_prefix

			-- And check for the layer_inner_suffix (".Cu") on the very right:
			and layer (layer'length - layer_inner_suffix'length + 1 .. layer'last) = layer_inner_suffix then

			-- Convert the characters between prefix and suffix to a layer id.
			-- If that fails, an exception is raised. see exception handler below.
				id := type_signal_layer_id'value (layer (
					layer'first + layer_inner_prefix'last 
					.. layer'last - layer_inner_suffix'length));

		-- All other layers are invalid:
		else
			invalid_layer;
		end if;
		
		return id;

		exception
			when constraint_error => invalid_layer; raise;
	end to_signal_layer_id;


	function to_pad_shape_circle (
		position	: in type_point_with_rotation;
		diameter	: in et_pcb.type_pad_size;
		offset		: in et_pcb_coordinates.type_point_2d)	-- the offset of the pad from the center
		return et_pcb.type_pad_outline is

		use et_pcb_coordinates;
		use et_pcb;
		use et_pcb.type_pad_circles;

		circle : type_pad_circle;

		shape : et_pcb.type_pad_outline; -- to be returned
	begin
		circle.center := type_point_2d (position);
		circle.radius := diameter / 2.0;
		move (circle.center, offset);
		append (shape.circles, circle);
		return shape;
	end to_pad_shape_circle;

	function to_pad_shape_rectangle (
	-- Converts the given position and dimensions of a rectangular pad
	-- to a list with four lines (top, bottom, right, left).
	-- CS: rework as in to_pad_shape_oval
		center		: in type_point_with_rotation; -- the pad center position (incl. angle)
		size_x		: in et_pcb.type_pad_size;	-- the size in x of the pad
		size_y		: in et_pcb.type_pad_size;	-- the size in y of the pad
		offset		: in et_pcb_coordinates.type_point_2d)	-- the offset of the pad from the center
		return et_pcb.type_pad_outline is

		use et_pcb;
		use et_pcb_coordinates;
		use geometry;
		use et_pcb.type_pad_lines;

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
		p11, p12 : type_point_2d;
		p21, p22 : type_point_2d;

		-- These are the four lines we need for the rectangular pad contour:
		line_1, line_2 : type_pad_line; -- left line, right line
		line_3, line_4 : type_pad_line; -- upper line, lower line

	begin -- to_pad_shape_rectangle
		-- set supportive cornert points
		p11 := type_point_2d (set (x => xn, y => yp));
		p12 := type_point_2d (set (x => xn, y => yn));

		p21 := type_point_2d (set (x => xp, y => yp));
		p22 := type_point_2d (set (x => xp, y => yn));

		-- rotate supportive points
		rotate (p11, angle);
		rotate (p11, angle);

		rotate (p21, angle);
		rotate (p21, angle);

		-- move supportive points by given offset
		move (p11, offset);
		move (p12, offset);
		
		move (p21, offset);
		move (p22, offset);

		-- set left line
		line_1.start_point := p11;
		line_1.end_point := p12;

		-- set right line
		line_2.start_point := p21;
		line_2.end_point := p22;

		-- set upper line
		line_3.start_point := p11;
		line_3.end_point := p21;		

		-- set lower line
		line_4.start_point := p12;
		line_4.end_point := p22;
		
		-- build shape
		shape.lines.append (line_1);
		shape.lines.append (line_2);
		shape.lines.append (line_3);
		shape.lines.append (line_4);
		
		return shape;
	end to_pad_shape_rectangle;

	function to_pad_shape_oval (
	-- Converts the given position and dimensions of an oval pad
	-- to a list with two vertical lines and two arcs (rotation assumed zero).
		center		: in type_point_with_rotation; -- the pad center position (incl. angle)
		size_x		: in et_pcb.type_pad_size;	-- the size in x of the pad
		size_y		: in et_pcb.type_pad_size;	-- the size in y of the pad
		offset		: in et_pcb_coordinates.type_point_2d)	-- the offset of the pad from the center
		return et_pcb.type_pad_outline is

		use et_pcb;
		use et_pcb_coordinates;
		use geometry;
		use et_pcb.type_pad_lines;
		use et_pcb.type_pad_arcs;		

		shape : type_pad_outline; -- to be returned

		-- The given center of the pad also provides us with the angle of rotation:
		angle : constant type_rotation := rot (center);
		
		-- supportive frequently used values
		x1p : constant type_distance := size_x / 2;
		x1n : constant type_distance := -(x1p);

		y1p : constant type_distance := size_y / 2;
		y1n : constant type_distance := -(y1p);

		y2p : constant type_distance := y1p - x1p;
		y2n : constant type_distance := -(y2p);

		-- supportive points:
		p11, p12 : type_point_2d; -- start/end point of left line
		p21, p22 : type_point_2d; -- start/end point of right line
		p41, p42 : type_point_2d; -- center of upper/lower arc

		-- These are the two lines and the two arcs we need for the oval pad contour:
		line_1, line_2 : type_pad_line; -- left line, right line
		arc_1, arc_2 : type_pad_arc; -- upper arc, lower arc
		
	begin -- to_pad_shape_oval

		-- set supportive points
		p11 := type_point_2d (set (x => x1n, y => y2p));
		p12 := type_point_2d (set (x => x1n, y => y2n));

		p21 := type_point_2d (set (x => x1p, y => y2p));
		p22 := type_point_2d (set (x => x1p, y => y2n));

		p41 := type_point_2d (set (x => zero, y => y2p));
		p42 := type_point_2d (set (x => zero, y => y2n));			

		-- rotate supportive points 
		rotate (p11, angle);
		rotate (p12, angle);
		
		rotate (p21, angle);
		rotate (p22, angle);
		
		rotate (p41, angle);
		rotate (p42, angle);		

		-- move supportive points by given offset
		move (p11, offset);
		move (p12, offset);
		
		move (p21, offset);
		move (p22, offset);
		
		move (p41, offset);
		move (p42, offset);		
		
		-- set left line
		line_1.start_point := p11;
		line_1.end_point := p12;

		-- set right line
		line_2.start_point := p21;
		line_2.end_point := p22;

		-- set upper arc
		arc_1.center := p41;
		arc_1.start_point := p11;
		arc_1.end_point := p21;

		-- set lower arc
		arc_2.center := p42;
		arc_2.start_point := p12;
		arc_2.end_point := p22;

		-- build shape
		shape.lines.append (line_1);
		shape.lines.append (line_2);
		shape.arcs.append (arc_1);
		shape.arcs.append (arc_2);				
		
		return shape;
	end to_pad_shape_oval;
	
	function to_pad_milling_contour (
	-- Converts the given position and dimensions of a rectangular slotted hole
	-- to a list with four lines (top, bottom, right, left).
		center		: in type_point_with_rotation; -- the terminal position (incl. angle, (z axis ignored))
		size_x		: in et_pcb.type_pad_size;	-- the size in x of the hole
		size_y		: in et_pcb.type_pad_size;	-- the size in y of the hole
		offset		: in et_pcb_coordinates.type_point_2d)	-- the offset of the pad from the center
		return et_pcb.type_pcb_contour_lines.list is

		use et_pcb;
		use et_pcb_coordinates;
		use geometry;

		lines : type_pcb_contour_lines.list; -- to be returned

		-- The given center of the pad also provides us with the angle of rotation:
		angle : constant type_rotation := rot (center);
		
		-- supportive frequently used values
		xp : constant type_distance := size_x / 2;
		xn : constant type_distance := -(xp);

		yp : constant type_distance := size_y / 2;
		yn : constant type_distance := -(yp);

 		-- supportive corner points:
		p11, p12 : type_point_2d;
		p21, p22 : type_point_2d;

		-- These are the four lines we need for the rectangular pad contour:
		line_1, line_2 : type_pcb_contour_line; -- left line, right line
		line_3, line_4 : type_pcb_contour_line; -- upper line, lower line

	begin -- to_pad_milling_contour
		-- set supportive cornert points
		p11 := type_point_2d (set (x => xn, y => yp));
		p12 := type_point_2d (set (x => xn, y => yn));

		p21 := type_point_2d (set (x => xp, y => yp));
		p22 := type_point_2d (set (x => xp, y => yn));

		-- rotate supportive points
		rotate (p11, angle);
		rotate (p11, angle);

		rotate (p21, angle);
		rotate (p21, angle);

		-- move supportive points by given offset
		move (p11, offset);
		move (p12, offset);
		
		move (p21, offset);
		move (p22, offset);

		-- set left line
		line_1.start_point := p11;
		line_1.end_point := p12;

		-- set right line
		line_2.start_point := p21;
		line_2.end_point := p22;

		-- set upper line
		line_3.start_point := p11;
		line_3.end_point := p21;		

		-- set lower line
		line_4.start_point := p12;
		line_4.end_point := p22;

		-- build milling contour
		lines.append (line_1);
		lines.append (line_2);
		lines.append (line_3);
		lines.append (line_4);
		
		return lines;
	end to_pad_milling_contour;

	function to_package_model (
	-- Builds a package model from the given lines.
		file_name		: in string; -- S_0201.kicad_mod
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level)
		return type_package_library is
		
		use et_pcb;
		use et_pcb.type_lines;

		-- Extract the actual package name (like S_0201) from the given file name:
		package_name : et_libraries.type_component_package_name.bounded_string :=
			et_libraries.to_package_name (ada.directories.base_name (file_name)); 

		function path_and_file_name return string is
		-- returns the path and file name. used for error messages.
			use et_libraries;
		begin
			--return "file " & ada.directories.compose (
			--	to_string (library_group), file_name);
			return "file " & file_name;
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
		terminal_pad_shape_tht 	: type_pad_shape_tht;
		terminal_pad_shape_smt 	: type_pad_shape_smt;

		terminal_face 			: et_pcb_coordinates.type_face;
		terminal_drill_size		: type_drill_size; 
		terminal_hole_shape	: type_tht_hole_shape; -- for slotted holes
		terminal_milling_size_x	: type_pad_milling_size;  -- CS use a composite instead ?
		terminal_milling_size_y	: type_pad_milling_size; 
		terminal_pad_drill_offset : et_pcb_coordinates.type_point_2d;

		-- The center of an smt pad or the position of the drill of a tht pad:
		terminal_position	: type_point_with_rotation; 
		
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
		terminal_stop_mask : type_stop_mask_status;

		-- Here we collect all kinds of terminals after they have been built.
		terminals : et_pcb.type_terminals.map;



	-- TEXTS
		text : type_text_package;

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
			use et_libraries;
			use et_libraries.type_text_content;
			use et_pcb_coordinates;
			use geometry;
		
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
			
			log (text => "arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), level => log_threshold + 4);

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
								when 1 => text.dimensions.width := to_distance (to_string (arg));
								when 2 => text.dimensions.height := to_distance (to_string (arg));
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
							set (terminal_position, zero_rotation); -- angle is optionally provided as last argument. if not provided default to zero.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
								when 3 => 
									set (terminal_position, to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_FP_TEXT =>
							--text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set (text.position, zero_rotation);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => text.position, value => to_distance (to_string (arg)));
								when 3 => 
									--text.angle := to_angle (to_string (arg));
									set (text.position, to_angle (to_string (arg)));
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
			use et_libraries;

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

			procedure insert_fp_arc is begin
			-- Append the arc to the container corresponding to the layer. Then log the arc properties.
				
				-- compute end point of arc from center, start_point and angle
				arc.end_point := type_point_2d (arc_end_point (arc.center, arc.start_point, arc.angle));

				-- The angle of the arc and its layer are now discarded
				-- as the arc is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_arc
				-- is formed and appended to the list of silk screen circles.
				case arc.layer is
					when TOP_SILK =>
						silk_screen.top.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_silk_screen_properties (TOP, silk_screen.top.arcs.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_silk_screen_properties (BOTTOM, silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_assy_doc_properties (TOP, assy_doc.top.arcs.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_assy_doc_properties (BOTTOM, assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point,
							width		=> arc.width));
						arc_keepout_properties (TOP, keepout.top.arcs.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.arcs.append ((
							center 		=> arc.center,
							start_point	=> arc.start_point, 
							end_point	=> arc.end_point,
							width		=> arc.width));
						arc_keepout_properties (BOTTOM, keepout.bottom.arcs.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_copper_properties (TOP, copper.top.arcs.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_copper_properties (BOTTOM, copper.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						stop_mask.top.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_stop_mask_properties (TOP, stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_stop_mask_properties (BOTTOM, stop_mask.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						stencil.top.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_stencil_properties (TOP, stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						stencil.bottom.arcs.append ((et_pcb.type_arc_2d (arc) with arc.width));
						arc_stencil_properties (BOTTOM, stencil.bottom.arcs.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_arc;

			procedure insert_fp_circle is begin
			-- Append the circle to the container corresponding to the layer. Then log the circle properties.
				
				-- Compute the circle radius from its center and point at circle:
				circle.radius := distance (circle.center, circle.point);

				-- The point at the circle and its layer are now discarded
				-- as the circle is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_circle
				-- is formed and appended to the list of silk screen circles.
				case circle.layer is
					when TOP_SILK =>
						silk_screen.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_silk_screen_properties (TOP, silk_screen.top.circles.last, log_threshold + 1);
						
					when BOT_SILK =>
						silk_screen.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						
						circle_silk_screen_properties (BOTTOM, silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						assy_doc.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (TOP, assy_doc.top.circles.last, log_threshold + 1);
						
					when BOT_ASSY =>
						assy_doc.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (BOTTOM, assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						keepout.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
							-- center 		=> circle.center,
							-- radius		=> circle.radius,
							-- filled		=> circle.filled,
							-- fill_style	=> circle.fill_style,
							-- width		=> circle.width,
							-- hatching_line_width	=> <>,
							-- hatching_spacing	=> <>
							-- ));
						circle_keepout_properties (TOP, keepout.top.circles.last, log_threshold + 1);
						
					when BOT_KEEP =>
						keepout.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
							-- center 		=> circle.center,
							-- radius		=> circle.radius, -- line width discarded because this is keepout
							-- filled		=> circle.filled,
							-- fill_style	=> circle.fill_style));
						circle_keepout_properties (BOTTOM, keepout.bottom.circles.last, log_threshold + 1);

						
					when TOP_COPPER => 
						copper.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						
						circle_copper_properties (TOP, copper.top.circles.last, log_threshold + 1);

					when BOT_COPPER => 
						copper.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						
						circle_copper_properties (BOTTOM, copper.bottom.circles.last, log_threshold + 1);

						
					when TOP_STOP =>
						stop_mask.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (TOP, stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						stop_mask.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (BOTTOM, stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						--stencil.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width));
						stencil.top.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>));
						circle_stencil_properties (TOP, stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						--stencil.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width));
						stencil.bottom.circles.append ((et_pcb.type_circle_2d (circle) with circle.width, others => <>));
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
						keepout.top.lines.append ((line.start_point, line.end_point, line.width));
						line_keepout_properties (TOP, keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						keepout.bottom.lines.append ((line.start_point, line.end_point, line.width));
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

				-- this cursor points to the terminal inserted last
				terminal_cursor : et_pcb.type_terminals.cursor;
				-- This flag goes true once a terminal is to be inserted that already exists (by its name).
				terminal_inserted : boolean;

				shape : et_pcb.type_pad_outline;

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

									width_inner_layers 	=> terminal_copper_width_inner_layers,
									drill_size			=> terminal_drill_size
								));

						when OVAL => -- a milled hole
							terminals.insert (
								key 		=> terminal_name,
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
									technology 			=> THT,
									tht_hole			=> MILLED,
									position			=> terminal_position,

									-- The shape is the same on top and on bottom side.									
									pad_shape_tht		=> (top => shape, bottom => shape),

									width_inner_layers	=> terminal_copper_width_inner_layers,

									-- The plated millings of the hole is a list of lines.
									millings => (lines 	=> to_pad_milling_contour (
											center		=> terminal_position,
											size_x		=> terminal_milling_size_x,
											size_y		=> terminal_milling_size_y,
											offset		=> terminal_pad_drill_offset),

										-- KiCad does not allow arcs or circles for plated millings.
										others	=> <>)
								));
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

										-- The shape is the same on top and on bottom side.									
										pad_shape_tht		=> (top => shape, bottom => shape),

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
								-- Therefor the size in x serves as diameter.
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste
										));
								
						end case;

						init_stop_and_mask; -- relevant for SMT terminals only (stop mask always open, solder paste never applied)
				end case;

				if terminal_inserted then
					et_pcb.terminal_properties (
						terminal		=> et_pcb.type_terminals.element (terminal_cursor),
						name			=> et_pcb.type_terminals.key (terminal_cursor),
						log_threshold	=> log_threshold + 1);
				else
					log (ERROR, "duplicated terminal " & to_string (terminal_name) & " !", console => true);
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
				log (ERROR, "in " & path_and_file_name, console => true);
				log (ERROR, "no placeholder for component " 
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
			use et_pcb.type_terminals;
			cursor : et_pcb.type_terminals.cursor := terminals.first;
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
							log (WARNING, "in " & path_and_file_name &
								" majority of terminals is" & to_string (SMT) &
								number (smt_count) &
								"Package technology should be" & to_string (SMT) & " !");
						end if;

					when SMT =>
						if smt_count < tht_count then
							log (WARNING, "in " & path_and_file_name &
								" majority of terminals is" & to_string (THT) &
								number (tht_count) &
								"Package technology should be" & to_string (THT) & " !");
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
					package_contour			=> (others => <>), -- CS to be filled from 3d model
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
		use et_libraries;
		use et_general;
		use et_general.type_directory_entries;
		use et_pcb;

		-- V4 RELATED ------------------------------------------------------------------------------------------
		-- The directory search lists have been created on reading the project file.
		-- Set lib_dir_cursor to first directory.
		use et_kicad.type_project_lib_dirs;
		lib_dir_cursor : et_kicad.type_project_lib_dirs.cursor := et_kicad.search_list_project_lib_dirs.first;
	
		-- backup the directory of origin
		use type_directory_name;
		origin_directory : type_directory_name.bounded_string := to_bounded_string (current_directory);
	
		-- After fetching the names of the package libraries, their names
		-- are stored here. When processing the list we use the library_name_cursor.
		library_names : type_directory_entries.list;
		library_name_cursor : type_directory_entries.cursor;
		
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

			package_names : type_directory_entries.list;
			package_name_cursor : type_directory_entries.cursor;
			
			library_handle : ada.text_io.file_type;
			line : type_fields_of_line; -- a line of a package model

			use et_pcb.type_lines;
			lines : et_pcb.type_lines.list; -- all lines of a single package model

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
			while package_name_cursor /= type_directory_entries.no_element loop
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
				while lib_dir_cursor /= et_kicad.type_project_lib_dirs.no_element loop

					log (text => "in directory " & et_kicad.to_string (element (lib_dir_cursor)), level => log_threshold + 1);
					
					-- Scan for package library in directory indicated by lib_dir_cursor:
					library_names := directory_entries (
						target_directory	=> et_kicad.to_string (element (lib_dir_cursor)),  
						category			=> ada.directories.directory,
						pattern				=> package_library_pattern); -- *.pretty stuff

					-- If directory contains no packages, notify operator that there are no package libraries.
					-- Otherwise loop through the library names and create the libraries in container package_libraries.
					if is_empty (library_names) then
						log (WARNING, "no package libraries found in " &
							et_kicad.to_string (element (lib_dir_cursor)) & " !");
					else
						-- show number of package libraries found in the directory
						log (text => "found" & count_type'image (length (library_names)) & " libraries", level => log_threshold + 2);
						log_indentation_up;

						-- Loop through library_names and create the same-named empty libraries 
						-- in container package_libraries:
						library_name_cursor := library_names.first;
						while library_name_cursor /= type_directory_entries.no_element loop
							log (text => "reading " & element (library_name_cursor) & " ...", level => log_threshold + 2);

							-- create the (empty) library in container package_libraries
							type_libraries.insert (
								container	=> package_libraries,
								key			=> to_file_name (compose ( -- ../lbr/tht_packages/plcc.pretty 
										containing_directory	=> et_kicad.to_string (element (lib_dir_cursor)),
										name					=> element (library_name_cursor))),
								inserted	=> library_inserted,
								position	=> library_cursor,
								new_item	=> type_packages_library.empty_map);

							-- If library has been created already (by import of other project) then there
							-- is no need to read it again.
							if library_inserted then
								-- change in library (the kicad package library is just a directory like ../lbr/bel_ic.pretty)
								set_directory (compose (
									containing_directory	=> et_kicad.to_string (element (lib_dir_cursor)),
									name					=> element (library_name_cursor)));

								-- Read the library contents and store them in package_libraries where
								-- library_cursor is pointing to:
								type_libraries.update_element (
									container	=> package_libraries,
									position	=> library_cursor,
									process		=> read_packages'access);

								-- change back to directory of origin
								set_directory (et_pcb.to_string (origin_directory));
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
					set_directory (et_pcb.to_string (origin_directory));

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

	function to_string (layer : in type_layer_id) return string is
	-- returns the given layer id as string.
	begin
		return type_layer_id'image (layer);
	end to_string;

	function to_layer_id (layer : in string) return type_layer_id is
	-- Converts a string like B.CU or F.Fab to a kicad layer id (0..49)
		layer_id : type_layer_id; -- to be returned
	begin
		-- Translate non-signal layers:
		if layer = layer_top_solder_paste then layer_id := layer_top_solder_paste_id;
		elsif layer = layer_bot_solder_paste then layer_id := layer_bot_solder_paste_id;

		elsif layer = layer_top_stop_mask then layer_id := layer_top_stop_mask_id;
		elsif layer = layer_bot_stop_mask then layer_id := layer_bot_stop_mask_id;

		elsif layer = layer_top_silk_screen then layer_id := layer_top_silk_screen_id;
		elsif layer = layer_bot_silk_screen then layer_id := layer_bot_silk_screen_id;

		elsif layer = layer_top_keepout then layer_id := layer_top_keepout_id;
		elsif layer = layer_bot_keepout then layer_id := layer_bot_keepout_id;

		elsif layer = layer_top_assy_doc then layer_id := layer_top_assy_doc_id;
		elsif layer = layer_bot_assy_doc then layer_id := layer_bot_assy_doc_id;

		-- CS other layers like adhes, eco, margin, ...

		
		-- Translate signal layers
		else
			layer_id := to_signal_layer_id (layer);
		end if;
		
		return layer_id;
	end to_layer_id;

	function to_string (shape : in type_pad_shape_tht) return string is
	begin
		return latin_1.space & to_lower (type_pad_shape_tht'image (shape));
	end to_string;
	
	function to_string (shape : in type_pad_shape_smt) return string is
	begin
		return latin_1.space & to_lower (type_pad_shape_smt'image (shape));
	end to_string;
	
	function to_layer_name (name : in string) return type_layer_name.bounded_string is
	-- converts a layer name given as string to a bounded string
	begin
		return type_layer_name.to_bounded_string (name);
	end to_layer_name;

	function to_layer_meaning (meaning : in string) return type_layer_meaning is
	-- converts a layer meaning given as string to a bounded string		
	begin
		return type_layer_meaning'value (meaning);
	end to_layer_meaning;

	function default_component_reference return et_libraries.type_device_name is
	-- Returns a default device name with an empty prefix and and id 0.
	-- Used to initialize a component reference.	
		use et_libraries;
	begin
		return ((
			prefix		=> type_device_name_prefix.to_bounded_string (""),
			id			=> device_name_index_default,
			id_width	=> 1));
	end default_component_reference;
	
	function to_board (
		file_name		: in string; -- pwr_supply.kicad_pcb
		lines			: in et_pcb.type_lines.list;
		log_threshold	: in et_string_processing.type_log_level) 
		return et_kicad_pcb.type_board is

		board : et_kicad_pcb.type_board; -- to be returned
		
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
			SEC_ARC_SEGMENTS,
			SEC_AREA,
			SEC_AUX_AXIS_ORIGIN,
			SEC_AT,
			SEC_ATTR,
			SEC_ANGLE,
			SEC_CENTER,
			SEC_CLEARANCE,
			SEC_CONNECT_PADS,
			SEC_DESCR,
			SEC_DRAWINGS,
			SEC_DRILL,
			SEC_DRILLSHAPE,
			SEC_EDGE_WIDTH,
			SEC_EFFECTS,
			SEC_END,
			SEC_EXCLUDEEDGELAYER,
			SEC_FILL,
			SEC_FILLED_POLYGON,
			SEC_FONT,
			SEC_FP_ARC,
			SEC_FP_CIRCLE,
			SEC_FP_LINE,
			SEC_FP_TEXT,
			SEC_GENERAL,
			SEC_GR_ARC,		-- board outlines, edge cuts
			SEC_GR_CIRCLE,	-- board outlines, edge cuts
			SEC_GR_LINE,	-- board outlines, edge cuts
			SEC_GR_TEXT,
			SEC_HATCH,
			SEC_HOST,
			SEC_HPGLPENDIAMETER,
			SEC_HPGLPENNUMBER,
			SEC_HPGLPENOVERLAY,
			SEC_HPGLPENSPEED,			
			SEC_JUSTIFY,	-- for packages on back side (mirrored)
			SEC_KICAD_PCB,
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
			SEC_MIN_THICKNESS,
			SEC_MIRROR,
			SEC_NET,
			SEC_NET_CLASS,
			SEC_NET_NAME,
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
			SEC_POLYGON,
			SEC_PRIORITY,
			SEC_PSA4OUTPUT, 
			SEC_PSNEGATIVE,
			SEC_PTS,
			SEC_RADIUS,
			SEC_ROTATE,
			SEC_SCALE,
			SEC_SCALESELECTION,
			SEC_SEGMENT,
			SEC_SEGMENT_WIDTH,
			SEC_SETUP,
			SEC_SUBTRACTMASKFROMSILK,
			SEC_SIZE,
			--SEC_SOLDER_MASK_MARGIN,
			SEC_SMOOTHING,
			SEC_START,
			SEC_STATUS,
			SEC_TAGS,
			SEC_TEDIT,
			SEC_THERMAL_BRIDGE_WIDTH,
			SEC_THERMAL_GAP,
			SEC_TRACE_CLEARANCE,
			SEC_TRACE_MIN,
			SEC_TRACE_WIDTH,
			SEC_TRACKS,
			SEC_THICKNESS,
			SEC_TSTAMP,
			SEC_USEAUXORIGIN,
			SEC_USEGERBEREXTENSIONS,
			SEC_USEGERBERATTRIBUTES, -- came with V5, CS supported in V4 ?
			SEC_USEGERBERADVANCEDATTRIBUTES, -- came with V5, CS supported in V4 ?
			SEC_CREATEGERBERJOBFILE, -- came with V5, CS supported in V4 ?
			SEC_UVIAS_ALLOWED,
			SEC_UVIA_DIA,
			SEC_UVIA_DRILL,
			SEC_UVIA_MIN_DRILL,
			SEC_UVIA_MIN_SIZE,
			SEC_UVIA_SIZE,
			SEC_VIA,
			SEC_VERSION,
			SEC_VIA_DIA,
			SEC_VIA_DRILL,
			SEC_VIA_MIN_DRILL,
			SEC_VIA_MIN_SIZE,
			SEC_VIA_SIZE,
			SEC_VISIBLE_ELEMENTS,
			SEC_VIASONMASK,
			SEC_WIDTH,
			SEC_XY,
			SEC_XYZ,
			SEC_ZONE,
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

		-- SEGMENTS, VIAS, POLYGONS
		segment			: type_segment;
		via				: type_via;
		polygon 		: type_polygon; -- NOTE: kicad allows polygons in copper layers exclusively
		polygon_point	: et_pcb_coordinates.type_point_2d;

		-- Since SEC_POLYGON and SEC_FILLED_POLYGON have the same subsections (SEC_PTS/SEC_XY)
		-- the flag section_polygon_entered is required. When section SEC_XY is executed,
		-- the flag indicates whether it is about corner points or fill-points of the polygon.
		section_polygon_entered : boolean;
		
		-- PACKAGES
		package_name 			: et_libraries.type_component_package_name.bounded_string;
		package_library_name	: et_kicad_general.type_library_name.bounded_string;
		package_position		: et_pcb_coordinates.type_package_position;
		
		-- package_path			: et_kicad.type_timestamp; -- like /59F17F64/59F18F3E/5B852A16/5B851D80
		-- CS: This this the sheet path. Currently it is ignored, because no need for it.
		-- If the need arises, package_path must be a list of type_timestamp.
		-- This could be achieved similar to the approach used procedure add_alternative_reference in et_kicad.
		
		-- The majority of terminals dictates the package technology. The default is THT.
		package_technology 	: type_assembly_technology := THT;

		-- By default a package is something real (with x,y,z dimension)
		package_appearance 	: type_package_appearance := REAL;

		package_text 		: type_text_package;
		package_reference 	: et_libraries.type_device_name := default_component_reference;
		package_value 		: et_libraries.type_value.bounded_string;

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
		
		package_silk_screen		: et_pcb.type_silk_screen_package_both_sides;
		package_assy_doc		: et_pcb.type_assembly_documentation_package_both_sides;
		package_keepout			: et_pcb.type_keepout_both_sides;
		package_copper			: et_pcb.type_copper_package_both_sides;
		
		-- countours of a package as provided by the 3d model:
		package_contour			: et_pcb.type_package_contour; -- CS not assigned yet
		
	-- TERMINALS
		-- Temporarily we need lots of variables for terminal properties.
		-- Later when the final terminals are assigned to the package, these variables
		-- compose the final terminal.
		terminal_name 			: et_libraries.type_terminal_name.bounded_string;
		terminal_technology		: type_assembly_technology;
		terminal_pad_shape_tht 	: type_pad_shape_tht;
		terminal_pad_shape_smt 	: type_pad_shape_smt;

		terminal_face 			: et_pcb_coordinates.type_face;
		terminal_drill_size		: type_drill_size; 
		terminal_hole_shape		: type_tht_hole_shape; -- for slotted holes
		terminal_milling_size_x	: type_pad_milling_size;
		terminal_milling_size_y	: type_pad_milling_size;

		terminal_pad_drill_offset : et_pcb_coordinates.type_point_2d;
		
		-- The center of an smt pad or the position of the drill of a tht pad:		
		terminal_position	: type_point_with_rotation;
		
		pad_size_x : type_pad_size;
		pad_size_y : type_pad_size;		

		terminal_net_name	: type_net_name.bounded_string;
		terminal_net_id		: type_net_id_terminal;
	
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
		terminal_stop_mask : type_stop_mask_status;

		-- Here we collect all kinds of terminals after they have been built.
		-- NOTE: This is the type_terminals as specified in et_kicad_pcb ! (includes net names)
		terminals : type_terminals.map;




		-- OBJECTS DRAWN DIRECTLY IN THE BOARD (NON-PACKAGE STUFF)
		board_line 		: type_line;
		board_arc		: type_arc;
		board_circle	: type_circle;
		board_text		: type_text_board;

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
			terminal_net_name := to_net_name ("");
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
				log (text => "line " & to_string (current_line), level => log_threshold + 4);
			else
				-- This should never happen:
				log (ERROR, "in " & file_name, console => true);
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

			procedure invalid_section is begin
				log (ERROR, "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' ! (read section)", console => true);
				raise constraint_error;
			end invalid_section;

		begin -- read_section
			-- save previous section on stack
			sections_stack.push (section);

			-- the former actvie section name becomes the parent section name
			section.parent := section.name;

			-- CS provide log info on current section
			-- log (text => "section " & to_string (section.name), level => log_threshold + 1);
			
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
					log (ERROR, "expect subsection name !", console => true);
					raise constraint_error;
				end if;
			end if;
			
			-- This is the validation of a section regarding its parent section.
			-- If an invalid subsection occurs, raise alarm and abort.
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION | SEC_HOST | SEC_GENERAL | SEC_PAGE |
							SEC_LAYERS | SEC_SEGMENT | SEC_VIA | SEC_SETUP | SEC_NET | SEC_NET_CLASS |
							SEC_MODULE | SEC_GR_LINE | SEC_GR_ARC | SEC_GR_CIRCLE | SEC_GR_TEXT | SEC_ZONE => null;
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
							SEC_USEGERBERATTRIBUTES | -- CS: came with V5. supported in V4 ?
							SEC_USEGERBERADVANCEDATTRIBUTES | -- CS: came with V5. supported in V4 ?
							SEC_CREATEGERBERJOBFILE | -- CS: came with V5. supported in V4 ?							
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

				when SEC_FP_TEXT | SEC_GR_TEXT =>
					case section.name is
						when SEC_AT | SEC_LAYER | SEC_EFFECTS => null;
						when others => invalid_section;
					end case;

				when SEC_EFFECTS =>
					case section.name is
						when SEC_FONT | SEC_JUSTIFY => null;
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

				when SEC_SEGMENT =>
					case section.name is
						when SEC_START | SEC_END | SEC_LAYER | SEC_WIDTH | SEC_NET | SEC_STATUS | SEC_TSTAMP => null;
						when others => invalid_section;
					end case;

				when SEC_VIA =>
					case section.name is
						when SEC_AT | SEC_SIZE | SEC_DRILL | SEC_LAYERS | SEC_NET | SEC_STATUS => null;
						when others => invalid_section;
					end case;

				when SEC_ZONE =>
					case section.name is
						when SEC_NET | SEC_NET_NAME | SEC_LAYER | SEC_TSTAMP | SEC_HATCH | SEC_PRIORITY |
							SEC_CONNECT_PADS | SEC_MIN_THICKNESS | SEC_FILL => null;

						-- Since SEC_POLYGON and SEC_FILLED_POLYGON have the same subsections (SEC_PTS/SEC_XY)
						-- the flag section_polygon_entered is required. When section SEC_XY is executed,
						-- the flag indicates whether it is about corner points or fill-points of the polygon.
						when SEC_POLYGON		=> section_polygon_entered := true;
						when SEC_FILLED_POLYGON	=> section_polygon_entered := false;
						when others => invalid_section;
					end case;

				when SEC_CONNECT_PADS =>
					case section.name is
						when SEC_CLEARANCE => null;
						when others => invalid_section;
					end case;

				when SEC_FILL =>
					case section.name is
						when SEC_MODE | SEC_ARC_SEGMENTS | SEC_THERMAL_GAP | SEC_THERMAL_BRIDGE_WIDTH |
							SEC_SMOOTHING | SEC_RADIUS => null;
						when others => invalid_section;
					end case;
					
				when SEC_POLYGON | SEC_FILLED_POLYGON =>
					case section.name is
						when SEC_PTS => null;
						when others => invalid_section;
					end case;

				when SEC_PTS =>
					case section.name is
						when SEC_XY => null;
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
						log (ERROR, "in " & file_name, console => true);
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
			use et_libraries;
			use et_libraries.type_text_content;
			use et_pcb_coordinates;
			use geometry;
			
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

			procedure invalid_attribute is begin
				log (ERROR, "invalid attribute !", console => true);
				raise constraint_error;
			end invalid_attribute;

			procedure invalid_section is begin
				log (ERROR, "invalid subsection '" & to_string (section.name) 
					 & "' in parent section '" & to_string (section.parent) & "' ! (read argument)", console => true);
				raise constraint_error;
			end invalid_section;

			procedure invalid_file_format is begin
				log (ERROR, "invalid file format ! Expect format version " & pcb_file_format_version_4 & " !",
					 console => true);
				raise constraint_error;
			end invalid_file_format;

			procedure to_polygon_pad_connections (connect_style : in string) is
			-- Sets the connection style of pads.
			-- It is about entries in the "zone" section like:
			-- (connect_pads (clearance 0.8)) -- thermal_relief 
			-- (connect_pads thru_hole_only (clearance 0.8)) -- tht only
			-- (connect_pads yes (clearance 0.8)) -- solid
			-- (connect_pads no (clearance 0.8)) -- none
			begin -- CS use text constants for string comparing
				if connect_style = "no" then -- no connections at all
					polygon.pad_connection := NONE;
					polygon.pad_technology := SMT_AND_THT; -- does not matter in this case
				elsif connect_style = "yes" then -- solid connections
					polygon.pad_connection := SOLID;
					polygon.pad_technology := SMT_AND_THT; -- applies for SMT and THT
				elsif connect_style = "thru_hole_only" then -- thermals for THT
					polygon.pad_connection := THERMAL;
					polygon.pad_technology := THT_ONLY;
				else -- thermals for SMT and THT
					polygon.pad_connection := THERMAL;
					polygon.pad_technology := SMT_AND_THT;
				end if;
			end to_polygon_pad_connections;

			procedure to_polygon_hatch_style (hatch_style : in string) is
			-- Sets the polygon hatch style.
			begin -- CS use function to_string (hatch_style) or to_hatch_style (hatch_style)
				if hatch_style = to_lower (type_polygon_hatch'image (NONE)) then
					polygon.gui_hatch_style := NONE;
				elsif hatch_style = to_lower (type_polygon_hatch'image (EDGE)) then
					polygon.gui_hatch_style := EDGE;
				elsif hatch_style = to_lower (type_polygon_hatch'image (FULL)) then
					polygon.gui_hatch_style := FULL;
				end if;
			end to_polygon_hatch_style;

			procedure test_pcbnew_version (version : in string) is
			-- in V4 the line looks like: 
			--  (kicad_pcb (version 4) (host pcbnew 4.0.7)
			-- or in v5 like:
			-- (kicad_pcb (version 20171130) (host pcbnew 5.0.0-5.0.0)
				procedure invalid_pcbnew_version (version : in string) is begin
					log (ERROR, "invalid " & host_name_pcbnew & " version ! Expect " & version & " !",
						console => true);
					raise constraint_error;
				end invalid_pcbnew_version;

				use et_import;
			begin -- test_pcbnew_version
				case cad_format is
					when KICAD_V4 =>
						if version /= pcb_new_version_4_0_7 then
							invalid_pcbnew_version (pcb_new_version_4_0_7);
						end if;
						
					when KICAD_V5 =>
						-- This check only makes sense if we have a real board file:
						if not board.dummy then
							
							-- CS: do a more professional range check here:
							if version /= pcb_new_version_5_0_0 then
								invalid_pcbnew_version (pcb_new_version_5_0_0);
							end if;
						end if;
						
					when others => raise constraint_error;
				end case;
			end test_pcbnew_version;

			procedure test_hostname (name : in string) is
			-- in V4 the line looks like: 
			--  (kicad_pcb (version 4) (host pcbnew 4.0.7)
			-- or in v5 like:
			-- (kicad_pcb (version 20171130) (host pcbnew 5.0.0-5.0.0)
				procedure invalid_host_name is begin
					log (ERROR, "invalid host name ! Expect " & host_name_pcbnew & " !",
						console => true);
					raise constraint_error;
				end invalid_host_name;

				use et_import;
			begin -- test_hostname
				case cad_format is
					when KICAD_V4 =>
						if name /= host_name_pcbnew then
							invalid_host_name;
						end if;

					when KICAD_V5 =>
						if name /= host_name_pcbnew then

							-- Newly created projects without a board may have a single 
							-- strange entry like:
							--  (kicad_pcb (version 4) (host kicad "dummy file") )
							if name = host_name_pcbnew_dummy_v5 then
								log (text => "dummy board file found", level => log_threshold + 1);
								board.dummy := true; -- signal other operations that this is a dummy file
							else
								invalid_host_name;
							end if;
						end if;

					when others => raise constraint_error;
				end case;
			end test_hostname;

			procedure test_format (format : in string) is
				use et_import;
			begin
				case cad_format is
					when KICAD_V4 =>
						-- the line looks like: (kicad_pcb (version 4) (host pcbnew 4.0.7)
						if to_string (arg) /= pcb_file_format_version_4 then
							invalid_file_format;
						end if;

					when KICAD_V5 =>
						-- the line looks like: 
						--  (kicad_pcb (version 20171130) (host pcbnew 5.0.0-5.0.0)
						-- newly created projects without a board have a line like:
						--  (kicad_pcb (version 4) (host kicad "dummy file") )
						-- CS test if a positive greater 20171130 is here.
						null;

					when others =>
						raise constraint_error;
				end case;
			end test_format;
						
			
		begin -- read_arg
			-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
			if element (current_line, character_cursor) = latin_1.quotation then
				-- Read the quotation-wrapped argument (strip quotations)

				-- get position of last character (before trailing quotation)
				end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

				-- if no trailing quotation found -> error
				if end_of_arg = -1 then
					log (ERROR, affected_line (element (line_cursor))
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
			
			log (text => "arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), level => log_threshold + 4);

			-- Validate arguments according to current section and the parent section.
			-- Load variables. When a section closes, the variables are used to build an object. see exec_section.
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION =>
							-- example: (kicad_pcb (version 4) (host pcbnew 4.0.7)
							case section.arg_counter is
								when 0 => null;
								when 1 => test_format (to_string (arg)); -- version 4 or 20171130
								when others => too_many_arguments;
							end case;

						when SEC_HOST =>
							case section.arg_counter is
								when 0 => null;
								when 1 => test_hostname (to_string (arg)); -- pcbnew 
									-- This sets the dummy_file flag if the board file is a dummy.
								
								when 2 => test_pcbnew_version (to_string (arg)); -- 5.0.0
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
								when 1 => netlist_net.id := to_net_id (to_string (arg));
								when 2 => netlist_net.name := to_net_name (to_string (arg));
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

						when SEC_GR_TEXT =>
							case section.arg_counter is
								when 0 => null;
								when 1 => board_text.content := to_bounded_string (to_string (arg));	
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
									validate_track_clearance (to_distance (to_string (arg)));
									net_class.clearance := (to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_TRACE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_track_width (to_distance (to_string (arg)));
									net_class.track_width_min := (to_distance (to_string (arg)));
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
									net_class.net_names.append (to_net_name (to_string (arg)));
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
									check_timestamp (package_time_edit);
								when others => too_many_arguments;
							end case;

						when SEC_TSTAMP =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS check length
									package_time_stamp := type_timestamp (to_string (arg));
									check_timestamp (package_time_stamp);
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
									set (axis => X, point => package_position, value => to_distance (to_string (arg)));
								when 2 =>
									set (axis => Y, point => package_position, value => to_distance (to_string (arg)));
								when 3 =>
									set (package_position, to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_PATH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null;
									-- the path is given like this /59F207B1/4564523. The forward slash must be removed:
									-- CS: no need yet. package_path := et_kicad.type_timestamp (to_string (arg)(2..package_path'length + 1));
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
							-- CS package_text.hidden := false; -- "hide" flag is optionally provided as last argument. if not, default to false
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

											package_reference := et_kicad.to_component_reference (to_string (arg));
											
										when VALUE =>
											-- The value (like 220R) is both the text content and the value itself.
										
											-- CS length check
											package_text.content := to_bounded_string (to_string (arg));
											-- CS character check

											if not value_length_valid (to_string (arg)) then 
												null; -- CS write something useful
											end if;
											
											package_value := et_libraries.to_value (to_string (arg));
											
											if not value_characters_valid (package_value) then
												null; -- CS write something useful
											end if;
											
										when USER =>
											-- CS length check
											package_text.content := to_bounded_string (to_string (arg));
											-- CS character check
									end case;
									
								when 3 => 
									if to_string (arg) = keyword_fp_text_hide then
										-- CS package_text.hidden := true;
										null;
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
							--package_text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set (package_text.position, zero_rotation);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => package_text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_text.position, value => to_distance (to_string (arg)));
								when 3 => 
									--package_text.angle := to_angle (to_string (arg));
									set (package_text.position, to_angle (to_string (arg)));
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
				when SEC_GR_TEXT =>
					case section.name is
						when SEC_AT =>
							--board_text.angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set (board_text.position, zero_rotation);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => board_text.position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_text.position, value => to_distance (to_string (arg)));
								when 3 => 
									--board_text.angle := to_angle (to_string (arg));
									set (board_text.position, to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									-- Translate the kicad layer names like B.Cu or F.SilkS to layer id (0..49)
									board_text.layer := to_layer_id (to_string (arg));
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
								when 1 => 
									package_text.dimensions.width := to_distance (to_string (arg));
									board_text.dimensions.width := to_distance (to_string (arg));
								when 2 => 
									package_text.dimensions.height := to_distance (to_string (arg));
									board_text.dimensions.height := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_THICKNESS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									package_text.line_width := to_distance (to_string (arg));
									board_text.line_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_EFFECTS =>	
					case section.name is
						when SEC_JUSTIFY => 
							-- If a text is placed at the bottom side, it must be mirrored.
							-- Since this is natural and indicated by the layer (B.SilkS, B.Cu, ...) there is
							-- no need for the extra flag (justify mirror). So we just test if the
							-- keyword "mirror" is present.
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) /= keyword_fp_text_mirrored then
										log (ERROR, "expect keyword '" & keyword_fp_text_mirrored & "' !", console => true);
										raise constraint_error;
									end if;
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
									set (axis => X, point => package_line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_line.start_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => package_line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_line.end_point, value => to_distance (to_string (arg)));
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
									set (axis => X, point => package_circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_circle.center, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => package_circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_circle.point, value => to_distance (to_string (arg)));
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
									set (axis => X, point => package_arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_arc.center, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => package_arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => package_arc.start_point, value => to_distance (to_string (arg)));
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
									pad_size_x := to_distance (to_string (arg));
								when 2 => 
									validate_pad_size (to_distance (to_string (arg)));
									pad_size_y := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_AT =>
							--terminal_angle := zero_angle; -- angle is optionally provided as last argument. if not provided default to zero.
							set (terminal_position, zero_rotation);
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => terminal_position, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => terminal_position, value => to_distance (to_string (arg)));
								when 3 => 
									set (terminal_position, to_angle (to_string (arg)));
								when others => too_many_arguments;
							end case;
							
						when SEC_DRILL =>
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
								when 1 => terminal_net_id := to_net_id (to_string (arg));
								when 2 => terminal_net_name := to_net_name (to_string (arg));
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
								when 1 => set (axis => X, point => terminal_pad_drill_offset, value => to_distance (to_string (arg)));
								when 2 => set (axis => Y, point => terminal_pad_drill_offset, value => to_distance (to_string (arg)));
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

						when SEC_USEGERBERATTRIBUTES =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS: board.plot.user_gerber_attributes := type_plot_user_gerber_attributes'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_USEGERBERADVANCEDATTRIBUTES =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS: board.plot.user_gerber_advanced_attributes := type_plot_user_gerber_advanced_attributes'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_CREATEGERBERJOBFILE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => null; -- CS: board.plot.create_gerber_jobfile := type_plot_create_gerber_jobfile'value (to_string (arg));
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
								when 1 => board.general.nets := to_net_id (to_string (arg));
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
									set (axis => X, point => board_arc.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_arc.center, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => board_arc.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_arc.start_point, value => to_distance (to_string (arg)));
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
									set (axis => X, point => board_circle.center, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_circle.center, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => board_circle.point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_circle.point, value => to_distance (to_string (arg)));
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
									set (axis => X, point => board_line.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_line.start_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => board_line.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => board_line.end_point, value => to_distance (to_string (arg)));
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

				-- parent section
				when SEC_VIA =>	
					case section.name is
						when SEC_AT =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									set (axis => X, point => via.position, value => to_distance (to_string (arg)));
								when 2 =>
									set (axis => Y, point => via.position, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_SIZE =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									via.diameter_total := to_distance (to_string (arg)); -- drill diameter + 2 * restring !
								when others => too_many_arguments;
							end case;

						when SEC_DRILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									validate_drill_size (to_distance (to_string (arg)));
									via.diameter := to_distance (to_string (arg)); -- this is the drill diameter !
								when others => too_many_arguments;
							end case;

						when SEC_LAYERS =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- convert the start layer name to a layer id (incl. validation)
									via.layer_start := to_signal_layer_id (to_string (arg));
								when 2 =>
									-- convert the end layer name to a layer id (incl. validation)
									via.layer_end := to_signal_layer_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									via.net_id := to_net_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_STATUS =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									via.status := type_via_status.to_bounded_string (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;
							
				-- parent section
				when SEC_SEGMENT =>
					case section.name is
						when SEC_START =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => segment.start_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => segment.start_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;
					
						when SEC_END =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => segment.end_point, value => to_distance (to_string (arg)));
								when 2 => 
									set (axis => Y, point => segment.end_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when SEC_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									validate_track_width (to_distance (to_string (arg)));
									segment.width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- convert the layer name to a layer id (incl. validation)
									segment.layer := to_signal_layer_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									segment.net_id := to_net_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_TSTAMP =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									segment.timestamp := type_timestamp (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_STATUS =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									segment.status := type_segment_status.to_bounded_string (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;

					end case;

				-- parent section
				when SEC_ZONE =>
					case section.name is
						when SEC_NET =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.net_id := to_net_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_NET_NAME =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.net_name := to_net_name (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_LAYER =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- convert the layer name to a layer id (incl. validation)
									polygon.layer := to_signal_layer_id (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_TSTAMP =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									polygon.timestamp := type_timestamp (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_HATCH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => to_polygon_hatch_style (to_string (arg));
								when 2 => polygon.hatching_line_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_PRIORITY =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									-- CS validate priority
									polygon.priority_level := to_polygon_priority (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_CONNECT_PADS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									to_polygon_pad_connections (to_string (arg));
								when others => too_many_arguments;
							end case;
							
						when SEC_MIN_THICKNESS =>
							case section.arg_counter is
								when 0 => null;
								when 1 =>
									polygon.min_thickness := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_FILL =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = "yes" then polygon.filled := true; -- CS constant for "yes"
									else polygon.filled := false;
									end if;
								when others => too_many_arguments;
							end case;
							
						when others => invalid_section;
					end case;

				-- parent section
				when SEC_CONNECT_PADS =>
					case section.name is
						when SEC_CLEARANCE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.isolation_gap := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_FILL =>
					case section.name is
						when SEC_MODE =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									if to_string (arg) = "segment" then polygon.fill_mode_segment := true; -- CS use constant for "segment"
									else 
										log (ERROR, "expect argument 'segment' for fill mode !", console => true);
										raise constraint_error;
									end if;
									
								when others => too_many_arguments;
							end case;

						when SEC_ARC_SEGMENTS =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.arc_segments := natural'value (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_THERMAL_GAP =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.thermal_gap := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_THERMAL_BRIDGE_WIDTH =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.thermal_width := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_SMOOTHING =>
							case section.arg_counter is
								when 0 => null;
								when 1 => polygon.corner_easing := to_corner_easing (to_string (arg));
								when others => too_many_arguments;
							end case;

						when SEC_RADIUS =>
							case section.arg_counter is
								when 0 => null;
								-- the corner easing radius applies for both chamfer and fillet type.
								when 1 => polygon.easing_radius := to_distance (to_string (arg));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
					end case;

				-- parent section
				when SEC_PTS =>
					case section.name is
						when SEC_XY =>
							case section.arg_counter is
								when 0 => null;
								when 1 => 
									set (axis => X, point => polygon_point, value => to_distance (to_string (arg)));
								when 2 =>
									set (axis => Y, point => polygon_point, value => to_distance (to_string (arg)));
								when others => too_many_arguments;
							end case;

						when others => invalid_section;
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

				when others => null; -- Not all sections require arguments.
			end case;
			
			exception
				when event:
					others =>
						log (ERROR, "in " & file_name, console => true);
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
			use et_libraries;

			procedure invalid_layer_reference is begin
				log (ERROR, "reference " & to_string (package_reference) & " must be in a silk screen layer !", console => true);
				raise constraint_error;
			end invalid_layer_reference;

			procedure invalid_layer_value is begin
				log (WARNING, "value " & to_string (package_value) & " should be in a fabrication layer !");
			end invalid_layer_value;

			procedure invalid_layer_user is begin
				log (ERROR, "user text " & to_string (package_text.content) 
					 & " must be in a silk screen or fabrication layer !", console => true);
				raise constraint_error;
			end invalid_layer_user;

			procedure invalid_layer is begin
				log (ERROR, "invalid layer for this object !", console => true);
				raise constraint_error;
			end invalid_layer;

			procedure warn_on_missing_net is begin
			-- Warns operator if a terminal is not connected to a net.
				if length (terminal_net_name) = 0 then
					log (WARNING, to_string (package_reference) & latin_1.space
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
								stencil			=> package_stencil,
								route_restrict	=> (others => <>), -- kicad does not know route restrict
								via_restrict	=> (others => <>), -- kicad does not know via restrict
								assembly_documentation	=> package_assy_doc,
								pcb_contour		=> (others => <>), -- kicad does not allow pcb contours in a package,
								
								--pcb_contour_plated	=> (others => <>), -- kicad does not allow plated pcb contours in a package
								-- CS: currently no need
								
								package_contour		=> package_contour
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
								stencil			=> package_stencil,
								route_restrict	=> (others => <>), -- kicad does not know route restrict
								via_restrict	=> (others => <>), -- kicad does not know via restrict
								assembly_documentation	=> package_assy_doc,
								pcb_contour		=> (others => <>) -- kicad does not allow pcb contours in a package
								
								--pcb_contour_plated	=> (others => <>) -- kicad does not allow plated pcb contours in a package
								-- CS: currently no need
								
								-- a virtual package does not have contours
								)
							);
					
				end case;

				-- abort if package already in board file, otherwise log coordinates and properties
				if package_inserted then

					-- log package coordinates
					log (text => "package " & to_string (package_reference)
						 & et_pcb.package_position (package_position), -- this is a function that returns package coordinates !
						 level => log_threshold + 1);
					
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
					package_reference := default_component_reference;
					package_value := to_value ("");

					-- delete list of terminals
					terminals.clear;

					-- clear silk screen
					package_silk_screen.top.lines.clear;
					package_silk_screen.top.arcs.clear;
					package_silk_screen.top.circles.clear;
					package_silk_screen.top.texts.clear;
					package_silk_screen.top.placeholders.clear;

					package_silk_screen.bottom.lines.clear;
					package_silk_screen.bottom.arcs.clear;
					package_silk_screen.bottom.circles.clear;
					package_silk_screen.bottom.texts.clear;
					package_silk_screen.bottom.placeholders.clear;

					-- clear assembly documentation
					package_assy_doc.top.lines.clear;
					package_assy_doc.top.arcs.clear;
					package_assy_doc.top.circles.clear;
					package_assy_doc.top.texts.clear;
					package_assy_doc.top.placeholders.clear;

					package_assy_doc.bottom.lines.clear;
					package_assy_doc.bottom.arcs.clear;
					package_assy_doc.bottom.circles.clear;
					package_assy_doc.bottom.texts.clear;
					package_assy_doc.bottom.placeholders.clear;

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
					log (ERROR, "package " & to_string (package_reference) 
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
					log (text => "layer id" & type_layer_id'image (layer_id) 
						 & " name " & type_layer_name.to_string (layer.name)
						 & " meaning " & type_layer_meaning'image (layer.meaning), level => log_threshold + 2);
				else
					log (ERROR, "layer id" & type_layer_id'image (layer_id) & " already used !", 
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
					log (text => "net class " & to_string (net_class_name), level => log_threshold + 1);
					
					-- Clean up list of net names for next net class.
					-- CS: We assume, all other components of net_class are provided in 
					-- next net class section and thus become overwritten.
					net_class.net_names.clear;
				else
					log (ERROR, "net class '" & to_string (net_class_name) & "' already defined !", console => true);
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
						log (text => "net id" & to_string (netlist_net.id) & " name " 
							& et_general.to_string (netlist_net.name),
							level => log_threshold + 1);
					end if;
				else
					log (ERROR, "either net id" & to_string (netlist_net.id) 
						& " or net name '" & et_general.to_string (netlist_net.name) & "' already used !",
						 console => true);
					raise constraint_error;
				end if;
					
			end insert_net;

			procedure insert_board_arc is begin
				-- Compute the arc end point from its center, start point and angle.
				-- Later the angle is discarded.
				board_arc.end_point := type_point_2d (arc_end_point (
					board_arc.center, board_arc.start_point, board_arc.angle));

				-- The board_arc is converted back to its anchestor and
				-- depending on the layer extended with specific properties.
				case board_arc.layer is
					when TOP_SILK =>
						board.silk_screen.top.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_silk_screen_properties (TOP, board.silk_screen.top.arcs.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_silk_screen_properties (BOTTOM, board.silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_assy_doc_properties (TOP, board.assy_doc.top.arcs.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_assy_doc_properties (BOTTOM, board.assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						board.stencil.top.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_stencil_properties (TOP, board.stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						board.stencil.bottom.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_stencil_properties (BOTTOM, board.stencil.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						board.stop_mask.top.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_stop_mask_properties (TOP, board.stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.arcs.append ((et_pcb.type_arc_2d (board_arc) with board_arc.width));
						arc_stop_mask_properties (BOTTOM, board.stop_mask.bottom.arcs.last, log_threshold + 1);


					when TOP_KEEP =>
						board.keepout.top.arcs.append ((
							center 		=> board_arc.center, 
							start_point	=> board_arc.start_point,
							end_point	=> board_arc.end_point,
							width		=> board_arc.width));
						arc_keepout_properties (TOP, board.keepout.top.arcs.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.arcs.append ((
							center 		=> board_arc.center, 
							start_point	=> board_arc.start_point,
							end_point	=> board_arc.end_point,
							width		=> board_arc.width));
						arc_keepout_properties (BOTTOM, board.keepout.bottom.arcs.last, log_threshold + 1);
						
					when EDGE_CUTS =>
						board.contour.arcs.append ((et_pcb.type_arc_2d (board_arc) with locked => NO));
						arc_pcb_contour_properties (board.contour.arcs.last, log_threshold + 1);
						
					when others => invalid_layer;
				end case;
			end insert_board_arc;

			procedure insert_board_circle is begin
				-- Compute the circle radius from its center and point at circle.
				-- Later the angle is discarded.
				board_circle.radius := distance (board_circle.center, board_circle.point);

				-- The board_circle is converted back to its anchestor and
				-- depending on the layer extended with specific properties.
				case board_circle.layer is
					when TOP_SILK =>
						board.silk_screen.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_silk_screen_properties (TOP, board.silk_screen.top.circles.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_silk_screen_properties (BOTTOM, board.silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (TOP, board.assy_doc.top.circles.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (BOTTOM, board.assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						--board.stencil.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width));
						board.stencil.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						circle_stencil_properties (TOP, board.stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						--board.stencil.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width));
						board.stencil.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						circle_stencil_properties (BOTTOM, board.stencil.bottom.circles.last, log_threshold + 1);
						

					when TOP_STOP =>
						board.stop_mask.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (TOP, board.stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (BOTTOM, board.stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						board.keepout.top.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						--board.keepout.top.circles.append ((
						--	center		=> board_circle.center,
						--	radius		=> board_circle.radius,
						--	filled		=> board_circle.filled,
						--	fill_style	=> board_circle.fill_style));
						circle_keepout_properties (TOP, board.keepout.top.circles.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.circles.append ((et_pcb.type_circle_2d (board_circle) with board_circle.width, others => <>));
						--board.keepout.bottom.circles.append ((
						--	center		=> board_circle.center,
						--	radius		=> board_circle.radius,
						--	filled		=> board_circle.filled,
						--	fill_style	=> board_circle.fill_style));
						circle_keepout_properties (BOTTOM, board.keepout.bottom.circles.last, log_threshold + 1);

						
					when EDGE_CUTS =>
						board.contour.circles.append ((et_pcb.type_circle_2d (board_circle) with locked => NO));
						circle_pcb_contour_properties (board.contour.circles.last, log_threshold + 1);
						
					when others => invalid_layer;
				end case;

			end insert_board_circle;

			procedure insert_board_line is begin
				-- The board_line is converted back to its anchestor, and
				-- depending on the layer, extended with specific properties.
				case board_line.layer is

					when TOP_SILK =>
						board.silk_screen.top.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_silk_screen_properties (TOP, board.silk_screen.top.lines.last, log_threshold + 1);

					when BOT_SILK =>
						board.silk_screen.bottom.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_silk_screen_properties (BOTTOM, board.silk_screen.bottom.lines.last, log_threshold + 1);

						
					when TOP_ASSY =>
						board.assy_doc.top.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_assy_doc_properties (TOP, board.assy_doc.top.lines.last, log_threshold + 1);

					when BOT_ASSY =>
						board.assy_doc.bottom.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_assy_doc_properties (BOTTOM, board.assy_doc.bottom.lines.last, log_threshold + 1);


					when TOP_PASTE =>
						board.stencil.top.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_stencil_properties (TOP, board.stencil.top.lines.last, log_threshold + 1);

					when BOT_PASTE =>
						board.stencil.bottom.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_stencil_properties (BOTTOM, board.stencil.bottom.lines.last, log_threshold + 1);

						
					when TOP_STOP =>
						board.stop_mask.top.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_stop_mask_properties (TOP, board.stop_mask.top.lines.last, log_threshold + 1);

					when BOT_STOP =>
						board.stop_mask.bottom.lines.append ((et_pcb.type_line_2d (board_line) with board_line.width));
						line_stop_mask_properties (BOTTOM, board.stop_mask.bottom.lines.last, log_threshold + 1);


					when TOP_KEEP =>
						board.keepout.top.lines.append ((
							start_point	=> board_line.start_point,
							end_point	=> board_line.end_point,
							width		=> board_line.width));
						line_keepout_properties (TOP, board.keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						board.keepout.bottom.lines.append ((
							start_point	=> board_line.start_point,
							end_point	=> board_line.end_point,
							width		=> board_line.width));
						line_keepout_properties (BOTTOM, board.keepout.bottom.lines.last, log_threshold + 1);

						
					when EDGE_CUTS =>
						board.contour.lines.append ((et_pcb.type_line_2d (board_line) with locked => NO));
						line_pcb_contour_properties (board.contour.lines.last, log_threshold + 1);

					when others => invalid_layer;
				end case;
					
			end insert_board_line;

			procedure insert_board_text is begin
			-- Inserts the board_text in the board. 
			-- According to the kicad layer, the text is appended to the silk_screen, assy_doc, copper ...
			-- of the board.
				case board_text.layer is
					when layer_top_silk_screen_id =>
						board.silk_screen.top.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_silk_screen_properties (TOP, board.silk_screen.top.texts.last, log_threshold + 1);
						
					when layer_bot_silk_screen_id =>
						board.silk_screen.bottom.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_silk_screen_properties (BOTTOM, board.silk_screen.bottom.texts.last, log_threshold + 1);

						
					when layer_top_assy_doc_id =>
						board.assy_doc.top.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_assy_doc_properties (TOP, board.assy_doc.top.texts.last, log_threshold + 1);
						
					when layer_bot_assy_doc_id =>
						board.assy_doc.bottom.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_assy_doc_properties (BOTTOM, board.assy_doc.bottom.texts.last, log_threshold + 1);

						
					when layer_top_stop_mask_id =>
						board.stop_mask.top.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_stop_mask_properties (TOP, board.stop_mask.top.texts.last, log_threshold + 1);
						
					when layer_bot_stop_mask_id =>
						board.stop_mask.bottom.texts.append ((et_pcb.type_text (board_text) with board_text.content));
						text_stop_mask_properties (BOTTOM, board.stop_mask.bottom.texts.last, log_threshold + 1);

					when others =>

						-- If text is placed in a kicad signal layer (copper) it is added to the list of
						-- texts in board.copper. The kicad layer id is translated to the ET layer id.
						-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)

						-- If text is placed in other (non-signal) layers -> error
						if board_text.layer in type_signal_layer_id then
							board.copper.texts.append ((
								et_pcb.type_text (board_text) with
									content => board_text.content,
									layer => et_pcb.type_signal_layer ((board_text.layer) + 1)
								));
							text_copper_properties (board.copper.texts.last, log_threshold + 1);
						else
							-- CS currently there is no reason for texts in stencil, keepout, glue or other layers.
							-- This would cause an error:
							log (ERROR, "Text not allowed in this layer !", console => true);
							-- CS output the layer by its full kicad name like B.SilkS or T.CU.
							-- This requires a function that translates from type_layer_id to layer_top_solder_paste ... 
							-- see layer name declarations in spec of this package
							raise constraint_error;
						end if;
				end case;
			end insert_board_text;
			
			procedure insert_fp_arc is begin
			-- Append the arc to the container corresponding to the layer. Then log the arc properties.

				-- compute end point of arc from center, start_point and angle
				package_arc.end_point := type_point_2d (
					arc_end_point (package_arc.center, package_arc.start_point, package_arc.angle));

				-- The angle of the arc and its layer are now discarded
				-- as the package_arc is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_arc
				-- is formed and appended to the list of silk screen circles.
				case package_arc.layer is
					when TOP_SILK =>
						package_silk_screen.top.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_silk_screen_properties (TOP, package_silk_screen.top.arcs.last, log_threshold + 1);
						
					when BOT_SILK =>
						package_silk_screen.bottom.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_silk_screen_properties (BOTTOM, package_silk_screen.bottom.arcs.last, log_threshold + 1);

						
					when TOP_ASSY =>
						package_assy_doc.top.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_assy_doc_properties (TOP, package_assy_doc.top.arcs.last, log_threshold + 1);
						
					when BOT_ASSY =>
						package_assy_doc.bottom.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_assy_doc_properties (BOTTOM, package_assy_doc.bottom.arcs.last, log_threshold + 1);

						
					when TOP_KEEP =>
						package_keepout.top.arcs.append ((
							center 		=> package_arc.center,
							start_point	=> package_arc.start_point, 
							end_point	=> package_arc.end_point,
							width		=> package_arc.width));
						arc_keepout_properties (TOP, package_keepout.top.arcs.last, log_threshold + 1);
						
					when BOT_KEEP =>
						package_keepout.bottom.arcs.append ((
							center 		=> package_arc.center,
							start_point	=> package_arc.start_point, 
							end_point	=> package_arc.end_point,
							width		=> package_arc.width));
						arc_keepout_properties (BOTTOM, package_keepout.bottom.arcs.last, log_threshold + 1);

						
					when TOP_COPPER => 
						package_copper.top.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_copper_properties (TOP, package_copper.top.arcs.last, log_threshold + 1);

					when BOT_COPPER => 
						package_copper.bottom.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_copper_properties (BOTTOM, package_copper.bottom.arcs.last, log_threshold + 1);

						
					when TOP_STOP =>
						package_stop_mask.top.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_stop_mask_properties (TOP, package_stop_mask.top.arcs.last, log_threshold + 1);

					when BOT_STOP =>
						package_stop_mask.bottom.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_stop_mask_properties (BOTTOM, package_stop_mask.bottom.arcs.last, log_threshold + 1);

						
					when TOP_PASTE =>
						package_stencil.top.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_stencil_properties (TOP, package_stencil.top.arcs.last, log_threshold + 1);

					when BOT_PASTE =>
						package_stencil.bottom.arcs.append ((et_pcb.type_arc_2d (package_arc) with package_arc.width));
						arc_stencil_properties (BOTTOM, package_stencil.bottom.arcs.last, log_threshold + 1);

					when others => invalid_layer;
				end case;

			end insert_fp_arc;
			
			procedure insert_fp_circle is begin
			-- Append the circle to the container corresponding to the layer. Then log the circle properties.

				-- Compute the circle radius from its center and point at circle:
				package_circle.radius := 
					distance (package_circle.center, package_circle.point);

				-- The point at the circle and its layer are now discarded
				-- as the package_circle is converted back to its anchestor
				-- and then extended with the line width. Thus a type_silk_circle
				-- is formed and appended to the list of silk screen circles.
				case package_circle.layer is
					when TOP_SILK =>
						package_silk_screen.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_silk_screen_properties (TOP, package_silk_screen.top.circles.last, log_threshold + 1);
						
					when BOT_SILK =>
						package_silk_screen.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_silk_screen_properties (BOTTOM, package_silk_screen.bottom.circles.last, log_threshold + 1);

						
					when TOP_ASSY =>
						package_assy_doc.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (TOP, package_assy_doc.top.circles.last, log_threshold + 1);
						
					when BOT_ASSY =>
						package_assy_doc.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>)); 
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_assy_doc_properties (BOTTOM, package_assy_doc.bottom.circles.last, log_threshold + 1);

						
					when TOP_KEEP =>
						package_keepout.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						--package_keepout.top.circles.append ((
						--	center 		=> package_circle.center,
						--	radius		=> package_circle.radius,  -- line width discarded because this is keepout
						--	filled		=> package_circle.filled,
						--	fill_style	=> package_circle.fill_style));
						circle_keepout_properties (TOP, package_keepout.top.circles.last, log_threshold + 1);
						
					when BOT_KEEP =>
						package_keepout.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						--package_keepout.bottom.circles.append ((
						--	center 		=> package_circle.center,
						--	radius		=> package_circle.radius,  -- line width discarded because this is keepout
						--	filled		=> package_circle.filled,
						--	fill_style	=> package_circle.fill_style));
						circle_keepout_properties (BOTTOM, package_keepout.bottom.circles.last, log_threshold + 1);

						
					when TOP_COPPER => 
						package_copper.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assume default.

						circle_copper_properties (TOP, package_copper.top.circles.last, log_threshold + 1);

					when BOT_COPPER => 
						package_copper.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assume default.

						circle_copper_properties (BOTTOM, package_copper.bottom.circles.last, log_threshold + 1);

						
					when TOP_STOP =>
						package_stop_mask.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (TOP, package_stop_mask.top.circles.last, log_threshold + 1);

					when BOT_STOP =>
						package_stop_mask.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						-- filling not supported by kicad -> therefore "others" assumes default.
						circle_stop_mask_properties (BOTTOM, package_stop_mask.bottom.circles.last, log_threshold + 1);

						
					when TOP_PASTE =>
						--package_stencil.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width));
						package_stencil.top.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
						circle_stencil_properties (TOP, package_stencil.top.circles.last, log_threshold + 1);

					when BOT_PASTE =>
						--package_stencil.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width));
						package_stencil.bottom.circles.append ((et_pcb.type_circle_2d (package_circle) with package_circle.width, others => <>));
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
						package_keepout.top.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
						line_keepout_properties (TOP, package_keepout.top.lines.last, log_threshold + 1);

					when BOT_KEEP =>
						package_keepout.bottom.lines.append ((package_line.start_point, package_line.end_point, package_line.width));
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
			-- Insert a terminal in the list "terminals".
			-- This is layout related stuff.
			
				-- This cursor points to the last inserted terminal:
				terminal_cursor : et_kicad_pcb.type_terminals.cursor;
				-- This flag goes true once a terminal is to be inserted that already exists (by its name).
				terminal_inserted : boolean;

				shape : et_pcb.type_pad_outline;

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

									width_inner_layers 	=> terminal_copper_width_inner_layers,
									drill_size			=> terminal_drill_size,
									
									-- the pad is connected with a certain net
									net_name			=> terminal_net_name
								));

						when OVAL => -- a milled hole
							terminals.insert (
								key 		=> terminal_name,
								position	=> terminal_cursor,
								inserted	=> terminal_inserted,
								new_item 	=> (
									technology 			=> THT,
									tht_hole			=> MILLED,
									position			=> terminal_position,
									
									-- The shape is the same on top and on bottom side.									
									pad_shape_tht		=> (top => shape, bottom => shape),
									
									width_inner_layers	=> terminal_copper_width_inner_layers,

									-- The plated millings of the hole is a list of lines.
									millings => (lines 	=> to_pad_milling_contour (
											center		=> terminal_position,
											size_x		=> terminal_milling_size_x,
											size_y		=> terminal_milling_size_y,
											offset		=> terminal_pad_drill_offset),

										-- KiCad does not allow arcs or circles for plated millings.
										others	=> <>),

									-- the pad is connected with a certain net
									net_name			=> terminal_net_name
								));
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

										-- The shape is the same on top and on bottom side.									
										pad_shape_tht		=> (top => shape, bottom => shape),

										width_inner_layers	=> terminal_copper_width_inner_layers,
										drill_size			=> terminal_drill_size,

										-- the pad is connected with a certain net
										net_name			=> terminal_net_name
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
								-- Therefor the size in x serves as diameter.
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste,

										-- the pad is connected with a certain net
										net_name		=> terminal_net_name
										));
							
							when RECTANGULAR =>

								-- Calculate the rectangular pad shape.
								shape := to_pad_shape_rectangle (
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste,

										-- the pad is connected with a certain net
										net_name		=> terminal_net_name
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
										pad_shape		=> shape,
										face 			=> terminal_face,
										stop_mask		=> terminal_stop_mask,
										solder_paste	=> terminal_solder_paste,

										-- the pad is connected with a certain net
										net_name		=> terminal_net_name
										));
								
						end case;

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
					if length (terminal_net_name) > 0 then
						log (text => "connected with net " & et_general.to_string (terminal_net_name),
							level => log_threshold + 1);
					else
						log (text => "not connected", level => log_threshold + 1);
					end if;
					log_indentation_down;

					-- reset net name 
					init_terminal_net_name; -- in case the next terminal has no net connected

				else -- terminal could not be inserted
					log (ERROR, "duplicated terminal " & to_string (terminal_name) & " !", console => true);
					raise constraint_error;
				end if;
					
			end insert_terminal;

			procedure insert_fp_text is begin
			
				-- Since there is no alignment information provided, use default values:
				package_text.alignment := (horizontal => CENTER, vertical => BOTTOM);

				case package_text.meaning is
					when REFERENCE =>

						-- Insert the properties of package_text in the list of text placeholders of silk screen.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). 
						-- The meaning of package_text is passed separately (via "with" statement).
						-- The content of package_text is discarded here. Only text properties and position matter:
						case package_text.layer is
							when TOP_SILK =>
								package_silk_screen.top.placeholders.append (
									(et_pcb.type_text (package_text) with meaning => REFERENCE));
								placeholder_silk_screen_properties (TOP, package_silk_screen.top.placeholders.last, log_threshold + 1);
								
							when BOT_SILK =>
								package_silk_screen.bottom.placeholders.append (
									(et_pcb.type_text (package_text) with meaning => REFERENCE));
								placeholder_silk_screen_properties (BOTTOM, package_silk_screen.bottom.placeholders.last, log_threshold + 1);

							when others => -- should never happen
								invalid_layer_reference; 
						end case;

					when VALUE =>

						-- Insert the properties of package_text in the list of text placeholders of assembly documentation.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). 
						-- The meaning of package_text is passed separately (via "with" statement).
						-- The content of package_text is discarded here. Only text properties and position matter:
						case package_text.layer is
							when TOP_ASSY =>
								package_assy_doc.top.placeholders.append (
									(et_pcb.type_text (package_text) with meaning => VALUE));
								placeholder_assy_doc_properties (TOP, package_assy_doc.top.placeholders.last, log_threshold + 1);
								
							when BOT_ASSY =>
								package_assy_doc.bottom.placeholders.append (
									(et_pcb.type_text (package_text) with meaning => VALUE));
								placeholder_assy_doc_properties (BOTTOM, package_assy_doc.bottom.placeholders.last, log_threshold + 1);
								
							when others => -- should never happen
								invalid_layer_value;
						end case;

					when USER =>

						-- Insert the user text in the list of texts of silk screen or assembly documentation.
						-- In order to get the basic properties of package_text it must be
						-- converted back to its anchestor (type_text). The content of package_text
						-- is passed separately (via "with" statement).
						-- User specific texts may be placed in silk screen or assembly documentation.
						-- Here the text properties and the content matter. Since there is no way to distinguish
						-- these texts they are NOT threated as placeholders. Their content is stored in the list
						-- of texts (of silk screen or assembly documentation):
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

			procedure insert_segment is
			-- inserts a segment in the list "segments"
			begin
				type_segments.append (
					container	=> board.segments,
					new_item	=> segment);

				log (text => "segment " & to_string (et_pcb.type_line_2d (segment)) & -- start and end point
					 " width" & to_string (segment.width) &
					 " layer" & to_string (segment.layer) &
					 " net_id" & to_string (segment.net_id) &
					 " status " & type_segment_status.to_string (segment.status),
					 -- CS status should be decoded and detailled output. 
					 -- see -- see https://forum.kicad.info/t/meaning-of-segment-status/10912/1
					 level => log_threshold + 1);
				
			end insert_segment;

			procedure insert_via is
			-- inserts a via in the list "vias"
			begin
				if via.layer_start > via.layer_end then
					log (ERROR, "via start layer id must be less than end layer id !", console => true);
					raise constraint_error;
				end if;
			
				type_vias.append (
					container	=> board.vias,
					new_item	=> via);

				log (text => "via" & to_string (et_pcb.type_drill (via)) & -- position and drill diameter
					" diameter_total" & to_string (via.diameter_total) &
					" layer_start" & to_string (via.layer_start) &
					" layer_end" & to_string (via.layer_end) &
					" net_id" & to_string (via.net_id) &
					" status " & type_via_status.to_string (via.status),
					 -- CS status should be decoded and detailled output. 
					 -- see -- see https://forum.kicad.info/t/meaning-of-segment-status/10912/1
					level => log_threshold + 1);
			end insert_via;

			procedure add_polygon_corner_point is
			-- adds the current polygon_point to the corner points of the current polygon
				use type_polygon_points;
				point_cursor : type_polygon_points.cursor;
				inserted : boolean := true;
			begin
				polygon.corners.insert (
					new_item	=> polygon_point,
					inserted	=> inserted,
					position	=> point_cursor
					);

				if inserted then
					log (text => "polygon corner point at" & to_string (polygon_point), level => log_threshold + 3);
				else
					log (ERROR, "multiple polygon corner points at" & to_string (polygon_point), console => true);
					raise constraint_error;
				end if;
				
			end add_polygon_corner_point;

			procedure insert_polygon is
			-- inserts the current polygon in the list "polygons"
				use type_polygon_points;
			begin
				board.polygons.append (polygon);

				log (text => "polygon/zone net " & et_general.to_string (polygon.net_name) &
					 " " & text_polygon_signal_layer & to_string (polygon.layer) &
					 " timestamp " & string (polygon.timestamp) & -- CS use constant
					 " " & text_polygon_priority_level & et_pcb.to_string (polygon.priority_level) &
					 -- CS: hatch_style and hatch_width are related to the display mode in the GUI.
					 -- So there is no need to output this stuff here.
					 --" hatch_width" & to_string (polygon.hatch_width) & -- CS use constant for "hatch width" ?
					 --" hatch_style" & to_string (polygon.hatch_style) & -- CS use constant for "hatch stlye" ?
					 " min_thickness/" & text_polygon_width_min & to_string (polygon.min_thickness) &
					 " " & text_polygon_isolation_gap & to_string (polygon.isolation_gap) &
					 " filled " & boolean'image (polygon.filled) & -- CS use constant
					 " fill_mode_segment " & boolean'image (polygon.fill_mode_segment) &
					 " smooting/easing" & to_string (polygon.corner_easing) &
					 " " & text_polygon_easing_radius & to_string (polygon.easing_radius) &
					 " arc_segments" & natural'image (polygon.arc_segments) & -- CS use constant
					 " " & text_polygon_thermal_gap & to_string (polygon.thermal_gap) &
					 " " & text_polygon_thermal_width & to_string (polygon.thermal_width) &
					 " " & text_polygon_pad_connection & to_string (polygon.pad_connection) &
					 " " & text_polygon_pad_technology & to_string (polygon.pad_technology),
					 level => log_threshold + 3);

				-- CS log corner points
				-- CS log fill points

				-- Warn about floating polygons:
				if length (polygon.net_name) = 0 then
					log (WARNING, "Polygon without connection with any net found !");
				end if;

				-- Reset selectors of "polygon" (variable "polygon" is a scratch variable).
				-- Includes cleaning up corner points for next polygon. 
				polygon := (others => <>);
				
			end insert_polygon;
			
		begin -- exec_section
			log (text => process_section (section.name), level => log_threshold + 5);
			case section.parent is
				when SEC_KICAD_PCB =>
					case section.name is
						when SEC_VERSION =>
							-- In V5 the board file could be a dummy file with version 4 written in the header.
							-- CS: It would be confusing for the operator to show the file format here.
							--log (text => system_name & " version " & pcb_file_format_version_4, level => log_threshold + 1); 
							null;

						when SEC_HOST =>
							--log (text => "host " & host_name_pcbnew & " version " & pcb_new_version_4_0_7, level => log_threshold + 1);
							null;

						when SEC_GENERAL =>
							null; -- CS log general information

						when SEC_PAGE =>
							log (text => "paper size " & et_general.to_string (board.paper_size), level => log_threshold + 1);

						when SEC_LAYERS =>
							null; -- nothing to do. work already done on leaving SEC_LAYER_ID

						when SEC_SETUP =>
							null; -- CS board log setup (DRC stuff)

						when SEC_NET =>
							insert_net;
							
						when SEC_NET_CLASS =>
							insert_net_class; -- includes logging of net class settings

						when SEC_MODULE =>
							insert_package; -- in temporarily container "packages"

						when SEC_GR_ARC =>
							insert_board_arc;

						when SEC_GR_CIRCLE =>
							insert_board_circle;

						when SEC_GR_LINE =>
							insert_board_line;

						when SEC_GR_TEXT =>
							insert_board_text;
							
						when SEC_SEGMENT =>
							insert_segment;

						when SEC_VIA =>
							insert_via;

						when SEC_ZONE =>
							insert_polygon;
							
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
							null; -- CS log plot parameters (the one and only CAM job imprinted in the board)
						
						when others => null; -- CS
					end case;
					
				-- parent section
				when SEC_MODULE =>
					case section.name is
						when SEC_TEDIT =>
							log (text => "time edit  " & string (package_time_edit), level => log_threshold + 1);

						when SEC_TSTAMP =>
							log (text => "time stamp " & string (package_time_stamp), level => log_threshold + 1);
							
						when SEC_DESCR =>
							log (text => to_string (package_description, verbose => true), level => log_threshold + 1);
							
						when SEC_TAGS =>
							log (text => to_string (package_tags), level => log_threshold + 1);

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

				when SEC_PTS =>
					case section.name is
						when SEC_XY =>
							if section_polygon_entered then
								add_polygon_corner_point;
							else
								null; -- CS add_polygon_fill_point
								-- CS currently the fill points are not read and thus ignored.
							end if;
						
						when others => null;
					end case;

					
				when others => null;
			end case;

			-- restore previous section from stack
			section := sections_stack.pop;
			log (text => return_to_section (section.name), level => log_threshold + 5);
			
			exception
				when event:
					others =>
						log (ERROR, "in " & file_name, console => true);
						log (ERROR, affected_line (element (line_cursor)) 
							& to_string (element (line_cursor)), console => true);
						log (text => ada.exceptions.exception_message (event));
						raise;
			
		end exec_section;
		
	begin -- to_board
		log (text => "parsing/building board ...", level => log_threshold);
		log_indentation_up;

		sections_stack.init;

		-- get first line
		current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
		log (text => "line " & to_string (current_line), level => log_threshold + 4);

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
			log (ERROR, "in " & file_name, console => true);
			log (ERROR, "top level section not closed !", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;

		return board;
	end to_board;

	
	procedure read_board (
	-- Reads the board file. Copies general board stuff to the schematic module.
	-- Global module_cursor is expected to point to the schematic module.
		file_name 		: in string;
		log_threshold	: in et_string_processing.type_log_level) is

		board_handle : ada.text_io.file_type;
		line : type_fields_of_line; -- a line of the board file

		use et_pcb.type_lines;
		lines : et_pcb.type_lines.list; -- all lines of the board file

		-- Here the board data goes. 
		-- CS: If Kicad supports multi boards some day, this must become a list of boards.
		board : et_kicad_pcb.type_board;

		procedure merge_board_and_schematic (log_threshold : in type_log_level) is
		-- Merges the board with the schematic module.
		-- The board is specified in et_kicad_pcb.board.
		-- The schematic module is specified in et_schematic.type_module.
		-- The schematic module is indicated by the module_cursor.
		
		-- IMPORTANT: Kicad allows component packages in the layout file to be edited by the operator.
		-- As long as the position of reference and value is edited, everything is ok. But changing position
		-- of a pad or an element in the silk screen is no good idea. The package in the board file would
		-- then be no longer a copy of the generic package in the library ! 
		-- The procedure in the follwing DOES NOT detect local modifications of component packages in the layout.
		-- We assume, the operator has left the packages in the board file as they are: just copies of packages in
		-- the library.
		-- Only the package position, reference position and value position are read and assigned to the
		-- selectors "position" and "text_placeholders" of a schematic component. See specs et_schematic.type_component
		-- for details.
			use et_schematic;

			function to_net_name (
			-- Returns for the given component reference and terminal the name of the attached net.
			-- The information required is sotred in the terminals of a package.
			-- Example: (pad 1 smd rect (at -2.925 -3.81) (size 2 0.6) (layers F.Cu F.Paste F.Mask) (net 1 /IN))
				reference	: in et_libraries.type_device_name;	-- IC45
				terminal	: in et_libraries.type_terminal_name.bounded_string) -- G7
				return type_net_name.bounded_string is
				net : type_net_name.bounded_string; -- to be returned

				use type_packages_board;
				package_cursor : type_packages_board.cursor;

				use type_terminals;
				terminals : type_terminals.map;
				terminal_cursor : type_terminals.cursor;
				
			begin -- to_net_name
				-- Locate the given component in the board. If component does not
				-- exist in the board -> raise alarm and abort.
				package_cursor := board.packages.find (reference);
				
				if package_cursor /= type_packages_board.no_element then
					-- The component exists. The package cursor points to given component package.
					-- Load the terminals of the component package:
					terminals := element (package_cursor).terminals;

					-- Locate the given terminal in the terminals list and fetch
					-- the name of the connected net. 
					-- If the terminal does not exist -> raise alarm and abort
					terminal_cursor := terminals.find (terminal);
					if terminal_cursor /= type_terminals.no_element then -- terminal found
						net := element (terminal_cursor).net_name;
					else
						log (ERROR, "component reference " & et_libraries.to_string (reference) &
							" terminal " & et_libraries.to_string (terminal) &
							 " not found in board !",
							console => true);
						raise constraint_error;
					end if;
				else -- component package does not exist
					log (ERROR, "component reference " & et_libraries.to_string (reference) &
						 " not found in board !",
						 console => true);
					raise constraint_error;
				end if;
				
				return net;
			end to_net_name;

			
			procedure add_board_objects (
			-- Adds board objects to the schematic module.
				mod_name : in kicad_coordinates.type_submodule_name.bounded_string;
				module   : in out et_kicad.type_module) is

				-- The nets of the module are copied here (in their present state):
				use et_kicad.type_nets;
				nets 		: et_kicad.type_nets.map := module.nets;
				net_cursor	: et_kicad.type_nets.cursor := nets.first;
				
				net_id		: type_net_id; -- the net id used by kicad

				-- The components of the module are copied here (in their present state):
				use et_kicad.type_components_schematic;
				components			: et_kicad.type_components_schematic.map := module.components;
				component_cursor	: et_kicad.type_components_schematic.cursor := components.first;

				use type_packages_board;
				package_cursor		: type_packages_board.cursor;
				package_reference	: et_libraries.type_device_name;
				package_position	: et_pcb_coordinates.type_package_position;

				text_placeholders	: et_pcb.type_text_placeholders;

				function to_net_id (name : in type_net_name.bounded_string) return type_net_id is
				-- Converts the given net name to a net id.
					use type_netlist;
					net_cursor : type_netlist.cursor := board.netlist.first;
					id : type_net_id; -- to be returned
					
					use et_kicad.type_ports_with_reference;
					portlist : et_kicad.type_ports_with_reference.set;
					port : et_kicad.type_port_with_reference;
					terminal : et_libraries.type_terminal;
					net_name_in_board : type_net_name.bounded_string;
				begin -- to_net_id

					-- If the given net has a proper name (like MCU_CLK), then the net id
					-- can be obtained by just looking up board.netlist.
					if not anonymous (name) then -- net has an explicitely given name

						-- search the given net name in the board netlist:
						while net_cursor /= type_netlist.no_element loop
							if element (net_cursor).name = name then
								id := element (net_cursor).id;
								net_name_in_board := name;
								exit;
							end if;
							next (net_cursor);
						end loop;

						-- if the net could not be found, then board and schematic are not consistent -> error
						if length (net_name_in_board) = 0 then
							log (ERROR, "net '" & to_string (net_name => name) & "' not found in board !", console => true);
							raise constraint_error;
						end if;

						
					else -- The net has no explicitely given name. the name is something like N$56.
						portlist := et_kicad.real_components_in_net (module => mod_name, net => name, log_threshold => log_threshold + 4);
						-- Returns a list of component ports that are connected with the given net.

						-- Load the first port of the portlist. 
						-- Port contains the component reference (like IC45 and the port name like GPIO4).
						port := element (portlist.first);

						-- The physical terminal name must be obtained now:
						terminal := et_kicad.to_terminal (port, mod_name, log_threshold + 4);

						-- Terminal contains the component reference (like IC45) and the
						-- physical terminal name (like G7).
						-- Now the connected net can be looked for:
						net_name_in_board := to_net_name (port.reference, terminal.name);

						if length (net_name_in_board) > 0 then

						-- From the net_name_in_board the net id follows as:
							while net_cursor /= type_netlist.no_element loop
								if element (net_cursor).name = net_name_in_board then
									id := element (net_cursor).id;
									exit;
								end if;
								next (net_cursor);
							end loop;
						end if;
							
					end if;

					return id;
				end to_net_id;

				function route (net_id : in type_net_id) return et_pcb.type_route is
				-- Collects segments and vias by the given net_id and returns them as a type_route.
					route : et_pcb.type_route; -- to be returned
					use type_segments;
					segment_cursor : type_segments.cursor := board.segments.first;
					line : et_pcb.type_copper_line_pcb; -- an ET segment

					use type_vias;
					via_cursor : type_vias.cursor := board.vias.first;
					via : et_pcb.type_via; -- an ET via
					restring : et_pcb.type_restring_width;

					use type_polygons;
					polygon_cursor : type_polygons.cursor := board.polygons.first;

					use et_pcb_coordinates;
				begin -- route
					log_indentation_up;
					log (text => "segments, vias and polygons (signal layers in IPC notation (TOP..BOTTOM / 1..n):", level => log_threshold + 3);
					 
					-- Find all segments that have the given net_id.
					-- Append segments to route.lines.
					log_indentation_up;
					while segment_cursor /= type_segments.no_element loop
						if element (segment_cursor).net_id = net_id then

							-- copy start/end point and line width (by a conversion to the base type)
							line := (et_pcb.type_copper_line (element (segment_cursor)) with 

									-- Translate the kicad layer id to the ET signal layer:
									-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
									-- The bottom layer in kicad is always number 31. Top layer is number 0.
									-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
									layer => et_pcb.type_signal_layer (element (segment_cursor).layer + 1)

									-- CS Translate the locked and differential status
									-- CS locked => et_pcb.NO -- translate from segment status to locked status
									-- CS differential -- translate from segment status to differential status
									);

							route.lines.append (line); -- append the segment to the lines of the route
							et_pcb.route_line_properties (route.lines.last, log_threshold + 3);

						end if;
						
						next (segment_cursor);
					end loop;

					-- Log if the net has no routed segments.
					if et_pcb.type_copper_lines_pcb.is_empty (route.lines) then
						log (text => "no segments", level => log_threshold + 3);
					end if;
					
					-- Find all vias that have the given net_id.
					-- Append vias to route.vias
					while via_cursor /= type_vias.no_element loop
						if element (via_cursor).net_id = net_id then

							-- For converting a kicad via to an ET via, the restring must be calculated.
							-- It is the (total via diameter - drill diameter) divided by 2:
							restring := (element (via_cursor).diameter_total - element (via_cursor).diameter) / 2;
						
							-- copy position, drill diameter (by a conversion to the base type)
							via := (et_pcb.type_drill (element (via_cursor)) with 

									-- Translate the kicad layer id to the ET signal layer:
									-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
									-- The bottom layer in kicad is always number 31. Top layer is number 0.
									-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
									layer_start	=> et_pcb.type_signal_layer (element (via_cursor).layer_start + 1),
									layer_end 	=> et_pcb.type_signal_layer (element (via_cursor).layer_end + 1),

									-- Since kicad does not distinguish between restring in outer or inner layers
									-- both are assigned the same value here:
									restring_outer => restring,
									restring_inner => restring
									
									-- CS Translate the locked and differential status
									-- CS locked => et_pcb.NO -- translate from segment status to locked status
									-- CS differential -- translate from segment status to differential status
									);

							route.vias.append (via); -- append the via to the vias of the route
							et_pcb.route_via_properties (route.vias.last, log_threshold + 3);

						end if;
					
						next (via_cursor);
					end loop;

					-- Log if the net has no vias.
					if et_pcb.type_vias.is_empty (route.vias) then
						log (text => "no vias", level => log_threshold + 3);
					end if;

					-- Append polygons to route.polygons
					while polygon_cursor /= type_polygons.no_element loop
						if element (polygon_cursor).net_id = net_id then
						-- Transfer kicad polygon to et polygon:

							-- The polygon to be appended to route.polygons is a controlled type.
							-- Hence, there are selectors (properties) that exist (or do not exist)
							-- depending on the kind of pad_connection. See spec for type_copper_polygon_pcb
							-- for details.

							-- These properites of kicad polygons are discarded as there is no need for them:
							-- net_id, timestamp, hatch_style, hatch_width, filled, fill_mode_segment, arc_segments
							
							case element (polygon_cursor).pad_connection is
								when et_pcb.THERMAL =>
									route.polygons.append (
										new_item => (et_pcb.type_copper_polygon (element (polygon_cursor)) with
											pad_connection		=> et_pcb.THERMAL,
											width_min			=> element (polygon_cursor).min_thickness,

											-- Translate the kicad layer id to the ET signal layer:
											-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
											-- The bottom layer in kicad is always number 31. Top layer is number 0.
											-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
											layer 				=> et_pcb.type_signal_layer (element (polygon_cursor).layer + 1),

											-- This is the type depended stuff:
											thermal_technology	=> element (polygon_cursor).pad_technology,
											thermal_gap			=> element (polygon_cursor).thermal_gap,
											thermal_width		=> element (polygon_cursor).thermal_width
										));

								when et_pcb.SOLID =>
									route.polygons.append (
										new_item => (et_pcb.type_copper_polygon (element (polygon_cursor)) with
											pad_connection		=> et_pcb.SOLID,
											width_min			=> element (polygon_cursor).min_thickness,
											
											-- Translate the kicad layer id to the ET signal layer:
											-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
											-- The bottom layer in kicad is always number 31. Top layer is number 0.
											-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
											layer 				=> et_pcb.type_signal_layer (element (polygon_cursor).layer + 1),
											
											-- This is the type depended stuff:
											solid_technology	=> element (polygon_cursor).pad_technology
										));
								
								when et_pcb.NONE =>
									route.polygons.append (
										new_item => (et_pcb.type_copper_polygon (element (polygon_cursor)) with
											pad_connection		=> et_pcb.NONE,
											width_min			=> element (polygon_cursor).min_thickness,
											
											-- Translate the kicad layer id to the ET signal layer:
											-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
											-- The bottom layer in kicad is always number 31. Top layer is number 0.
											-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
											layer 				=> et_pcb.type_signal_layer (element (polygon_cursor).layer + 1)

											-- no further properties
										));

							end case;

							et_pcb.route_polygon_properties (route.polygons.last, log_threshold + 3);

						end if;
							
						next (polygon_cursor);
					end loop;
					
					log_indentation_down;
					log_indentation_down;	
					
					return route;
				end route;

				procedure add_route (
				-- adds routing information to the schematic module
					net_name	: in type_net_name.bounded_string;
					net			: in out et_kicad.type_net) is
				begin
					net.route := route (net_id);
				end add_route;

				procedure update_component_in_schematic (
				-- Updates the component in the schematic with position, text placeholders
					comp_ref	: in et_libraries.type_device_name;
					component	: in out et_kicad.type_component_schematic) is
				begin
					component.position := package_position;
					component.text_placeholders := text_placeholders;
				end update_component_in_schematic;

				function to_placeholders return et_pcb.type_text_placeholders is 
				-- Returns the placeholders for reference and value of the current package (indicated by package_cursor).
				-- The return distinguishes them by the face (TOP/BOTTOM), silk screen and assembly documentation.
					use et_pcb;
					use et_pcb_coordinates;
					placeholders : type_text_placeholders; -- to be returned

					procedure query_placeholders (
						comp_reference	: in et_libraries.type_device_name;
						comp_package	: in type_package_board) is

						use et_pcb.type_text_placeholders_package;

						-- points to a placeholder in the package
						cursor : et_pcb.type_text_placeholders_package.cursor;
					begin -- query_placeholders 
						-- Collect placeholders for REFERENCE in TOP silk screen:
						cursor := comp_package.silk_screen.top.placeholders.first;
						while cursor /= type_text_placeholders_package.no_element loop

							if element (cursor).meaning = REFERENCE then
	
								type_text_placeholders_package.append (
									container	=> placeholders.silk_screen.top,
									new_item	=> (et_pcb.type_text (element (cursor)) with meaning => REFERENCE));
	
								-- log placeholder properties
								placeholder_silk_screen_properties (TOP, placeholders.silk_screen.top.last, log_threshold + 3);
							end if;
							
							next (cursor);
						end loop;

						-- Collect placeholders for REFERENCE in BOTTOM silk screen:
						cursor := comp_package.silk_screen.bottom.placeholders.first;
						while cursor /= type_text_placeholders_package.no_element loop

							if element (cursor).meaning = REFERENCE then

								type_text_placeholders_package.append (
									container	=> placeholders.silk_screen.bottom,
									new_item	=> (et_pcb.type_text (element (cursor)) with meaning => REFERENCE));

								-- log placeholder properties
								placeholder_silk_screen_properties (BOTTOM, placeholders.silk_screen.bottom.last, log_threshold + 3);
							end if;
							
							next (cursor);
						end loop;

						-- Collect placeholders for VALUE in TOP assembly documentation:
						cursor := comp_package.assembly_documentation.top.placeholders.first;
						while cursor /= type_text_placeholders_package.no_element loop

							if element (cursor).meaning = VALUE then

								type_text_placeholders_package.append (
									container	=> placeholders.assy_doc.top,
									new_item	=> (et_pcb.type_text (element (cursor)) with meaning => VALUE));

								-- log placeholder properties
								placeholder_assy_doc_properties (TOP, placeholders.assy_doc.top.last, log_threshold + 3);
							end if;
							
							next (cursor);
						end loop;

						-- Collect placeholders for VALUE in BOTTOM assembly documentation:
						cursor := comp_package.assembly_documentation.bottom.placeholders.first;
						while cursor /= type_text_placeholders_package.no_element loop

							if element (cursor).meaning = VALUE then

								type_text_placeholders_package.append (
									container	=> placeholders.assy_doc.bottom,
									new_item	=> (et_pcb.type_text (element (cursor)) with meaning => VALUE));

								-- log placeholder properties
								placeholder_assy_doc_properties (BOTTOM, placeholders.assy_doc.bottom.last, log_threshold + 3);
							end if;
							
							next (cursor);
						end loop;
						
					end query_placeholders;
					
				begin -- to_placeholders
					log_indentation_up;

					query_element (
						position	=> package_cursor,
						process		=> query_placeholders'access);
					
					log_indentation_down;
					return placeholders;
					
				end to_placeholders;

				procedure transfer_net_classes is 
				-- net classes must be tranferred from board.net_classes to the schematic module
				-- A kicad net class has a name and a list of net_names
				-- whereas
				-- an ET net class has a just a name. In the schematic a particular net has the 
				-- class name as a property.
					use type_net_classes;
					net_class_cursor_board	: type_net_classes.cursor;

					use type_nets_of_class;
					nets_of_class		: type_nets_of_class.list;
					net_cursor_board	: type_nets_of_class.cursor;

					use et_kicad.type_nets;
					net_cursor_schematic : et_kicad.type_nets.cursor;

					function to_net_name (net_name_in : in type_net_name.bounded_string)
					-- Translates from an anonymous kicad net name like "Net-(IC2-Pad11)" to an 
					-- anonymous ET name like "N$45".
						return type_net_name.bounded_string is
						net_name_out : type_net_name.bounded_string; -- to be returned

						package_cursor	: type_packages_board.cursor := board.packages.first;
						package_name	: et_libraries.type_device_name;
						terminal_found	: boolean := false;
						terminal_name	: et_libraries.type_terminal_name.bounded_string;

						procedure query_terminals (
							package_name	: in et_libraries.type_device_name;
							packge			: in type_package_board) is
							use type_terminals;
							terminal_cursor : type_terminals.cursor := packge.terminals.first;
						begin -- query_terminals
							-- Loop in terminals of current package until a terminal
							-- is found that is connected with the given net name_in.
							-- On match, set the terminal_name, package_name and exit the loop.
							-- The flag terminal_found indicates the superordinated loop to exit prematurely.
							while terminal_cursor /= type_terminals.no_element loop
								if element (terminal_cursor).net_name = net_name_in then
									terminal_found := true;

									-- set terminal name
									terminal_name := key (terminal_cursor); -- E14
									-- set package_name (in superordinated function to_net_name):
									to_net_name.package_name := key (package_cursor); -- IC49
									
									exit;
								end if;
								next (terminal_cursor);
							end loop;
							
						end query_terminals;
							
					begin -- to_net_name
						log_indentation_up;
						log (text => "translating anonymous kicad net name " & et_general.to_string (net_name_in) & " to " &
							et_general.system_name & " name ... ", level => log_threshold + 3);

						-- Loop in packages until a suitable terminal has been found.
						while package_cursor /= type_packages_board.no_element and not terminal_found loop

							-- Query terminals of current package (query_terminals sets the flag terminal_found so that
							-- this loop ends prematurely once a suitable terminal has been found.
							query_element (
								position	=> package_cursor,
								process		=> query_terminals'access);
						
							next (package_cursor);
						end loop;

						if not terminal_found then
							log (ERROR, "net " & et_general.to_string (net_name_in) 
								 & " not connected to any package !", console => true);
							raise constraint_error;
						end if;

						-- Now we know: the given net_name_in is connected with package_name and terminal_name:
						-- package_name -- IC49
						-- terminal_name -- E14

						-- Get the name of the net connected with the given terminal:
						net_name_out := et_kicad.connected_net (mod_name, package_name, terminal_name, log_threshold + 4);

						log_indentation_up;
						log (text => "the " & et_general.system_name & " net name is " 
							 & et_general.to_string (net_name_out), level => log_threshold + 3);
						log_indentation_down;
						
						log_indentation_down;
						return net_name_out;
					end to_net_name;
						
					procedure set_net_class (
					-- Sets the class of the given net in the schematic module.
						net_name	: in type_net_name.bounded_string;
						net 		: in out et_kicad.type_net) is
					begin
						net.class := key (net_class_cursor_board);
						log (text => " net name " & et_general.to_string (net_name), level => log_threshold + 3);
					end set_net_class;
					
				begin -- transfer_net_classes
					-- Copy the net class settings from kicad-board to the schematic module:
					net_class_cursor_board := board.net_classes.first;
					while net_class_cursor_board /= type_net_classes.no_element loop -- loop in net classes of board
						log (text => "net class " & et_pcb.to_string (key (net_class_cursor_board)), level => log_threshold + 2);

						-- copy net class name and its basic properties
						module.net_classes.insert (
							key 		=> key (net_class_cursor_board), -- class name
							new_item	=> et_pcb.type_net_class (element (net_class_cursor_board))); -- properties

						-- From the board, get the net names of the current class:
						nets_of_class := element (net_class_cursor_board).net_names;

						-- In the schematic module: Set the current net class for all
						-- nets listed in nets_of_class:
						net_cursor_board := nets_of_class.first;
						while net_cursor_board /= type_nets_of_class.no_element loop -- loop in nets_of_class (in board)

							-- Locate the current net in the schematic module. Anonymous kicad names like
							-- "Net-(IC2-Pad11)" do not exist in the schematic module. If such a name is given 
							-- intentionally, it will be found by a regular "find in container" operation. 
							-- If it could not be found, it is an anonymous net. The name "Net-(IC2-Pad11)" must then
							-- be translated to the anonymous ET net name like "N$45".
							net_cursor_schematic := module.nets.find (element (net_cursor_board));
							if net_cursor_schematic = et_kicad.type_nets.no_element then
								-- anonymous net -> translate to ET notation
								net_cursor_schematic := module.nets.find (to_net_name (element (net_cursor_board)));
							end if;
						
							et_kicad.type_nets.update_element (
								container	=> module.nets, -- the current schematic module
								position	=> net_cursor_schematic, -- the current net
								process		=> set_net_class'access); -- set the net class
						
							next (net_cursor_board);
						end loop;
						
						next (net_class_cursor_board);
					end loop;

				end transfer_net_classes;

				procedure transfer_floating_polygons is
				-- Transfers floating polygons (their net_id is zero) to the schematic module (selector "board.copper.polygons").
					use type_polygons;
					polygon_cursor : type_polygons.cursor := board.polygons.first;
				begin
					-- search polygons with a net_id of zero:
					while polygon_cursor /= type_polygons.no_element loop
						if element (polygon_cursor).net_id = type_net_id'first then
						-- Transfer kicad polygon to et polygon:

							-- These properites of kicad polygons are discarded as there is no need for them:
							-- net_id, timestamp, hatch_style, hatch_width, filled, fill_mode_segment, arc_segments
							
							module.board.copper.polygons.append (
								new_item => (et_pcb.type_copper_polygon (element (polygon_cursor)) with
									width_min			=> element (polygon_cursor).min_thickness,

									-- Translate the kicad layer id to the ET signal layer:
									-- kicad signal layer are numbered from 0..31, ET signal layers are numbered from 1..n.
									-- The bottom layer in kicad is always number 31. Top layer is number 0.
									-- The kicad bottom copper layer becomes the ET signal layer 32 ! (NOT et_pcb.type_signal_layer'last !!)
									layer 				=> et_pcb.type_signal_layer (element (polygon_cursor).layer + 1)
								));

							et_pcb.floating_copper_polygon_properties (module.board.copper.polygons.last, log_threshold + 2);
							log (WARNING, "polygon is not connected with any net !", level => log_threshold + 2);

						end if;
						next (polygon_cursor);
					end loop;
					
				end transfer_floating_polygons;
				
			begin -- add_board_objects
				-- General board stuff (not related to any components) is
				-- copied right away:
				module.board.paper_size		:= board.paper_size;
				
				module.board.silk_screen	:= board.silk_screen;
				module.board.assy_doc		:= board.assy_doc;
				module.board.stencil 		:= board.stencil;
				module.board.stop_mask 		:= board.stop_mask;
				module.board.keepout 		:= board.keepout;
				module.board.contour 		:= board.contour;

				-- segments, vias and polygons (only those polygons that are connected with a net)
				log_indentation_up;
				while net_cursor /= et_kicad.type_nets.no_element loop

					-- We are interested in nets that have more than one terminal connected.
					-- Nets with less than two terminals do not appear in a kicad board file and must be skipped here.
					
					-- NOTE: Nets without explicitely given name are named like N$1, N$2, ... 
					-- The Kicad notation like "Net-(X1-Pad5)" is NOT used !!!
					-- The id of a name-less net (like N$5) can be obtained still, by looking up the terminals
					-- of a component package. See details in procedure to_net_id.
					if et_kicad.real_components_in_net (
						module			=> mod_name,
						net 			=> key (net_cursor),
						log_threshold	=> log_threshold + 4).length > 1 then

							-- log (text => "pre net " & to_string (key (net_cursor)), level => log_threshold + 2);
							net_id := to_net_id (key (net_cursor));
							log (text => "net " & et_general.to_string (key (net_cursor)) & " id" &
								 to_string (net_id), level => log_threshold + 2);

							-- add route (segments and vias) to module.nets (see et_schematic type_module)
							et_kicad.type_nets.update_element (
								container	=> module.nets,
								position	=> find (module.nets, key (net_cursor)),
								process		=> add_route'access);
					end if;

					next (net_cursor);
				end loop;

				-- transfer the kicad net classes to the schematic module
				transfer_net_classes;

				-- update package positions in schematic module
				while component_cursor /= et_kicad.type_components_schematic.no_element loop -- (cursor points to schematic components)

					-- We are interested in real components only. Virtual schematic components
					-- do not appear in a board and thus are skipped.
					if et_libraries."=" (element (component_cursor).appearance, et_libraries.sch_pcb) then

						-- set package reference as the component reference (from schematic)
						package_reference := key (component_cursor);
						--log (text => "component " & et_libraries.to_string (package_reference), level => log_threshold + 3);

						-- in the board: locate the package by the given package_reference:
						package_cursor := find (board.packages, package_reference);

						-- If the package exists, get package_position, verify value
						-- and update the schematic module with the package_position.
						-- Otherwise the package does not exist in the board -> error and abort
						if package_cursor /= type_packages_board.no_element then

							-- Make sure the value in schematic matches value in layout.
							-- On mismatch -> error and abort
							if et_libraries.type_value."=" (
								element (component_cursor).value, -- value in schematic
								element (package_cursor).value) then -- value in layout

								package_position := element (package_cursor).position;

								log (text => "package " & et_libraries.to_string (package_reference) &
									et_pcb.package_position (package_position), level => log_threshold + 2);

								-- Extract the text placeholders for reference and value from the 
								-- current package (indicated by package_cursor) and store them
								-- in text_placeholders. procedure update_component_in_schematic will
								-- later update the component in the schematic with text_placeholders.
								text_placeholders := to_placeholders;
								
								-- update component in schematic module
								et_kicad.type_components_schematic.update_element (
									container 	=> module.components,
									position	=> find (module.components, package_reference),
									process		=> update_component_in_schematic'access);

								
							else -- value mismatch
								log (ERROR, "value of " & et_libraries.to_string (package_reference) &
									 " mismatch ! In schematic: " & et_libraries.to_string (element (component_cursor).value) &
									 " in layout: " & et_libraries.to_string (element (package_cursor).value),
									console => true);
								raise constraint_error;
							end if;
								
						else -- package not found in layout
							log (ERROR, "package " & et_libraries.to_string (package_reference) &
								 " not found in the board !", console => true);
							raise constraint_error;
						end if;
							
					end if;

					next (component_cursor);
				end loop;

				transfer_floating_polygons;
				
				-- CS if export into ET requested by operator:
				-- CS export in CAM job file (source: board.plot) ?
				-- CS export in net class file (source: schematic module.net_classes) ?
				-- CS export in DRC file (source: board.setup) ?
				
				log_indentation_down;
				
			end add_board_objects;

			
		begin -- merge_board_and_schematic
			log (text => "merging board and schematic ...", level => log_threshold + 1);

			et_kicad.modules.update_element (
				position	=> et_kicad.module_cursor,
				process		=> add_board_objects'access);

			exception
				when event:
					others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_message (event), console => true);
						raise;
			
		end merge_board_and_schematic;

		procedure set_board_available_flag (
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is
		begin
			module.board_available := et_schematic.TRUE;
		end set_board_available_flag;
		
	begin -- read_board
		log (text => "reading board file " & file_name & " ...", level => log_threshold);
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

			-- parse and process the board data stored in "lines"
			board := to_board (file_name, lines, log_threshold + 1); -- board is a et_kicad_pcb.type_board

			-- merging board and schematic makes sense if the board file contains real board data:
			if not board.dummy then

				-- Now the board file has been read AND it is not a dummy:
				--  - Set the board_available flag in the module (By default it is cleared).
				et_kicad.modules.update_element (
					position	=> et_kicad.module_cursor,
					process		=> set_board_available_flag'access);

				-- do the merge
				merge_board_and_schematic (log_threshold + 1);
			end if;
			
		else
			log (text => "board file " & file_name & " not available. nothing to do.", level => log_threshold);
		end if;
		
		log_indentation_down;
	end read_board;

	procedure read_boards (log_threshold : in et_string_processing.type_log_level) is
	-- Imports layout files. The files to be imported are named after the schematic modules.
	-- The schematic modules are indicated by module_cursor.
		use et_kicad.type_modules;
		use et_string_processing;
		use ada.directories;
		use et_kicad;
	begin
		-- We start with the first module of the modules.
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= et_kicad.type_modules.no_element loop
			log (text => "module " & kicad_coordinates.to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
	
			-- read the layout file
			et_kicad_pcb.read_board (
				file_name => compose (
						name 		=> kicad_coordinates.to_string (key (module_cursor)),
						extension	=> file_extension_board),
				log_threshold 	=> log_threshold + 1);

			log_indentation_down;
			next (module_cursor);
		end loop;
	end read_boards;


	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in type_package_library_name.bounded_string;		-- ../lbr/bel_ic.pretty
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		terminal_port_map	: in et_libraries.type_terminal_port_map.map) 
		return boolean is

		use type_libraries;
		library_cursor : type_libraries.cursor;

		procedure validate_terminals (package_terminals : in et_pcb.type_terminals.map) is
		-- Test if the terminals of the terminal_port_map are also in the given package.
		-- Raises constraint_error if a terminal could not be found in the package.
			use et_pcb.type_terminals; -- the terminals of the package
			use et_libraries.type_terminal_port_map;
		
			-- This cursor points to the terminal in the terminal_port_map
			terminal_cursor : et_libraries.type_terminal_port_map.cursor; 

			-- For temporarily storage of a terminal name:
			terminal_name_in_map : et_libraries.type_terminal_name.bounded_string;
		begin -- validate_terminals
			-- Loop in terminal_port_map. Test each terminal whether it occurs
			-- in the package_terminals.
			terminal_cursor := terminal_port_map.first;
			while terminal_cursor /= et_libraries.type_terminal_port_map.no_element loop
				terminal_name_in_map := key (terminal_cursor);

				if package_terminals.find (terminal_name_in_map) = et_pcb.type_terminals.no_element then
					log (ERROR, "package " & et_libraries.to_string (packge => package_name)
						 & " does not have a terminal '" 
						 & et_libraries.to_string (terminal_name_in_map) & "' !", console => true);
					raise constraint_error;
				end if;
				
				next (terminal_cursor);
			end loop;
		end validate_terminals;
			
	
		procedure locate_package (
		-- Locates the package by package_name in the given package library.
			library_name	: in type_package_library_name.bounded_string;
			packages		: in type_packages_library.map) is
			package_cursor : type_packages_library.cursor;

			use type_packages_library;
			use et_pcb.type_terminals;
			use et_libraries.type_terminal_port_map;
			terminals : et_libraries.type_terminal_count;
		begin
			if is_empty (packages) then
				log (ERROR, "package library " & et_libraries.to_string (library_name)
					 & " is empty !", console => true);
				raise constraint_error;
			else
				-- locate the package
				package_cursor := packages.find (package_name);
				if package_cursor = type_packages_library.no_element then
					log (ERROR, "package " & et_libraries.to_string (packge => package_name)
						& " not found in library " & et_libraries.to_string (library_name)
						& " !", console => true);
					raise constraint_error;
				else
					-- load the total number of terminals the package provides
					terminals := et_libraries.type_terminal_count (length (element (package_cursor).terminals));

					-- If the package has less terminals than the given terminal_port_map abort:
					if et_libraries."<" (terminals, et_libraries.type_terminal_count (length (terminal_port_map))) then
						log (ERROR, "package " & et_libraries.to_string (packge => package_name)
							& " as too little terminals !",
							console => true);
						raise constraint_error;
					else
						validate_terminals (element (package_cursor).terminals);
					end if;
					
				end if;

			end if;
			
		end locate_package;
		
	begin -- terminal_port_map_fits
		if not is_empty (package_libraries) then
			library_cursor := package_libraries.find (library_name);

			if library_cursor = type_libraries.no_element then
				log (ERROR, "package library " & et_libraries.to_string (library_name)
					 --& " not found in " & et_libraries.to_string (et_libraries.library_group)
					 & " not found"
					 & " !", console => true);
				raise constraint_error;
			else
				-- locate the given package (by package_name) in the given package library:
				query_element (
					position	=> library_cursor,
					process		=> locate_package'access);
			end if;
				
		else
			log (ERROR, "no package libraries available !", console => true);
			raise constraint_error;
		end if;

		return true;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_port_map_fits;

	function terminal_count (
	-- Returns the number of terminals of the given package in the given library.
		packge : in type_package_library_name.bounded_string) -- ../lbr/bel_ic.pretty/S_SO14
		return et_libraries.type_terminal_count is

		library_name : type_package_library_name.bounded_string;
		package_name : et_libraries.type_component_package_name.bounded_string;
		
		use type_libraries;
		
		terminals : et_libraries.type_terminal_count; -- to be returned
		library_cursor : type_libraries.cursor; -- points to the library

		procedure locate_package (
			library_name	: in type_package_library_name.bounded_string;
			packages		: in type_packages_library.map) is
			use et_pcb.type_terminals;
			use type_packages_library;
			package_cursor : type_packages_library.cursor;
		begin
			-- locate the package
			package_cursor := packages.find (package_name);

			-- get number of terminals
			terminals := et_libraries.type_terminal_count (length (element (package_cursor).terminals));
		end locate_package;
		
	begin -- terminal_count

		-- extract the library and package name from the given package
		package_name := et_libraries.to_package_name (ada.directories.simple_name (et_libraries.to_string (packge))); -- S_SO14
		library_name := et_libraries.to_file_name (ada.directories.containing_directory (et_libraries.to_string (packge))); -- ../lbr/bel_ic.pretty
		
		-- locate the library
		library_cursor := type_libraries.find (package_libraries, library_name);

		if library_cursor = type_libraries.no_element then
			log (ERROR, et_libraries.to_string (library_name) & " not found !", console => true);
			raise constraint_error;
		else
			-- query packages in library
			type_libraries.query_element (library_cursor, locate_package'access);
		end if;
		
		return terminals;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_count;
	

-- 	procedure to_native (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Converts the packages (from package_libraries) to native packages.
-- 	-- NOTE: Packages of the board (incl. their deviations from the package_libraries) are ignored !
-- 	begin
-- 		log (text => "packages ...", level => log_threshold);
-- 		log_indentation_up;
-- -- CS
-- -- 
-- 		log_indentation_down;
-- 	end to_native;

	
end et_kicad_pcb;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
