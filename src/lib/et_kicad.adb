------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
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
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	
--		3. Make sure ports of netchangers are named like 1 or 2.

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_configuration;
with et_geometry;

with et_general;
with et_string_processing;		use et_string_processing;
with et_pcb;

package body et_kicad is

	procedure clear (lines : in out type_lines.list) is
	begin
		type_lines.clear (lines);
	end clear;

	procedure add (line : in type_fields_of_line) is
	begin
		lines.append (line);
	end add;
	
	function first (lines : in type_lines.list) return type_lines.cursor is
	begin
		return type_lines.first (lines);
	end first;

	procedure next (line : in out type_lines.cursor) is
	begin
		type_lines.next (line);
	end next;

	function line return et_string_processing.type_fields_of_line is
	begin
		return type_lines.element (line_cursor);
	end line;
	
	procedure invalid_field (line : in type_fields_of_line) is
	begin
		log_indentation_reset;
		log (message_error & affected_line (line) & "invalid field !", console => true);

		log (to_string (line), console => true);

		log ("Field indexes must be in range" 
			 & type_component_field_id'image (type_component_field_id'first)
			 & " .." 
			 & type_component_field_id'image (type_component_field_id'last)
			 & " !", 
			console => true);

		raise constraint_error;
	end invalid_field;

	procedure validate_prefix (prefix : in et_libraries.type_component_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_string_processing;
		use et_libraries.type_component_prefix;
	begin
		if to_string (prefix) = power_flag_prefix or to_string (prefix) = power_symbol_prefix then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix "
				 & to_string (prefix) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;

	procedure validate_prefix (reference : in et_libraries.type_component_reference) is
	-- Tests if the given reference has a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_libraries.type_component_prefix;
	begin
		if to_string (reference.prefix) = power_flag_prefix or to_string (reference.prefix) = power_symbol_prefix then
			null;
		else
			log_indentation_reset;
			log (message_error & "invalid prefix in component reference "
				 & et_libraries.to_string (reference) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;
			
	function to_point (x_in, y_in : in string) return type_2d_point is
		point : type_2d_point;
		x : type_distance_xy;
		y : type_distance_xy;
	begin
		x := mil_to_distance (x_in);
		y := mil_to_distance (y_in);

		set_x (point, x);
		set_y (point, y);
		return point;
	end to_point;
	
	function library_name (text : in string) return et_libraries.type_library_name.bounded_string is
	-- extracts from a string like "bel_ic:S_SO14" the library name "bel_ic"
		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line;
	begin
		return et_libraries.type_library_name.to_bounded_string (
			field (
				read_line (
					line => text,
					ifs => latin_1.colon), 
				position => 1) -- the part before the colon
				);
	end library_name;

	function package_name (text : in string) return et_libraries.type_component_package_name.bounded_string is
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"
		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line;
	begin
		return et_libraries.type_component_package_name.to_bounded_string (
			field (
				read_line (
					line => text,
					ifs => latin_1.colon), 
				position => 2) -- the part after the colon
				);
	end package_name;

	function to_text_meaning (
	-- Extracts from a scheamtic field like "F 0 "#PWR01" H 2000 3050 50  0001 C CNN" its meaning.
	-- Extracts from a component field like "F0 "IC" 0 50 50 H V C CNN" its meaning.
	-- Since the fields start different in libaray and schematic we also need a flag that tells
	-- the function whether we are dealing with schematic or library fields.
		line : in type_fields_of_line;
		schematic : in boolean) -- set false if it is about fields in a library, true if it is about a schematic field	
		return et_libraries.type_text_meaning is

		function field (
			line		: in type_fields_of_line;
			position	: in positive) return string renames get_field_from_line;
		
		meaning : et_libraries.type_text_meaning;

		function strip_f ( text : in string) return string is
		-- removes the heading character from the given string.
		begin return text(text'first+1..text'last); end strip_f;

		function strip_id ( text : in string) return string is
		-- removes the trailing id from the given string.
		begin return text(text'first..text'first); end strip_id;
	
	begin -- to_text_meaning
		case schematic is
			when true =>

				-- In a schematic the meaning of a text field is identified by "F 0 ...".

				-- So the first thing to do is test if the letter F at the begin of the line:
				if field (line,1) = component_field_identifier then

					-- Then we test the field id.
					-- The field id must be mapped to the actual field meaning:
					case type_component_field_id'value (field (line,2)) is -- "0..9"
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_package		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_purpose		=> meaning := et_libraries.purpose;
						when component_field_partcode		=> meaning := et_libraries.partcode;
						when component_field_commissioned	=> meaning := et_libraries.commissioned;
						when component_field_updated		=> meaning := et_libraries.updated;
						when component_field_author			=> meaning := et_libraries.author;
						when component_field_bom			=> meaning := et_libraries.bom;
						when others => invalid_field (line);
					end case;

				else
					invalid_field (line);
				end if;
				
			when false =>

				-- In a library the meaning of a text field is identified by "F0 .. F9".
				
				-- So the first thing to do is test if the letter F at the begin of the line:
				if strip_id (field (line,1)) = component_field_identifier then
				
					case type_component_field_id'value (strip_f (field (line,1))) is
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_package		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_purpose		=> meaning := et_libraries.purpose;
						when component_field_partcode		=> meaning := et_libraries.partcode;
						when component_field_commissioned	=> meaning := et_libraries.commissioned;
						when component_field_updated		=> meaning := et_libraries.updated;
						when component_field_author			=> meaning := et_libraries.author;
						when component_field_bom			=> meaning := et_libraries.bom;
						when others => invalid_field (line);
					end case;

				else
					invalid_field (line);
				end if;

		end case;

		return meaning;

		exception
			when constraint_error =>
				invalid_field (line); -- CS: May display the affected line a second time in some cases.
				raise;
		
	end to_text_meaning;
								 
	function to_field_orientation (text : in string) return et_coordinates.type_angle is
	-- Converts a kicad field text orientation character (H/V) to type_angle.
	begin	
		case type_field_orientation'value (text) is
			when H => return 0.0;
			when V => return 90.0;
		end case;

		exception 
			when constraint_error =>
				log_indentation_reset;
				log (message_error & "invalid text orientation !", console => true);
				raise;
			when others =>
				log_indentation_reset;
				log (message_error & "invalid text orientation !", console => true);
				raise;
	end to_field_orientation;
	
	function to_alignment_horizontal (text : in string) return et_libraries.type_text_alignment_horizontal is
	-- Converts a horizontal kicad text alignment to type_text_alignment_horizontal.
		a : et_libraries.type_text_alignment_horizontal;
	begin
		case type_field_alignment_horizontal'value(text) is
			when L => a := et_libraries.left;
			when C => a := et_libraries.center;
			when R => a := et_libraries.right;
		end case;
		return a;
	end to_alignment_horizontal;

	function to_alignment_vertical (text : in string) return et_libraries.type_text_alignment_vertical is
	-- Converts a vertical kicad text alignment to type_text_alignment_vertical.
	-- The given text is something like CNN. We are interested in the first character only.
		a : et_libraries.type_text_alignment_vertical;
		s : string (1..1) := text(text'first..text'first);
	begin
		case type_field_alignment_vertical'value(s) is
			when T => a := et_libraries.top;
			when C => a := et_libraries.center;
			when B => a := et_libraries.bottom;
		end case;
		return a;
	end to_alignment_vertical;

	function to_text_style (
	-- Converts a vertical kicad text style to type_text_style.
	-- The given style_in is something like CNN or "Italic" (if it is about a text field or a simple text).
	-- We are interested in the 2nd and 3rd character only.
		style_in : in string;
		text : in boolean -- true if it is about the style of a text, false if it is about the style of a field
		-- Explanation: The style of a text is something like "~" or "Italic".
		-- The style of a field comes with the letters 2 and 3 of a string like CNN.
		) return et_libraries.type_text_style is
		
		a : et_libraries.type_text_style;
		s_field : string (1..2);
	
		procedure invalid_style is
		begin
			log_indentation_reset;
			log (message_error & "invalid text style '" & style_in & "' !");
			raise constraint_error;
		end invalid_style;
		
	begin -- to_text_style
		case text is
			when true =>
				if style_in = text_schematic_style_normal then
					a := et_libraries.type_text_style'first;
				elsif style_in = text_schematic_style_italic then
					a := et_libraries.italic;
				else
					invalid_style;
				end if;
				
			when false =>
				s_field := style_in(style_in'first+1..style_in'last);
				
				if    s_field = field_style_default then 		a := et_libraries.type_text_style'first;
				elsif s_field = field_style_bold then 			a := et_libraries.bold;
				elsif s_field = field_style_italic then 		a := et_libraries.italic;
				elsif s_field = field_style_italic_bold then 	a := et_libraries.italic_bold;
				else
					invalid_style;
				end if;
		end case;

		return a;
		
		exception
			when constraint_error =>
				invalid_style;

				return a; -- CS: never reached
				
	end to_text_style;
	
	function to_field_visible ( 
	-- Converts the kicad field visible flag to the type_text_visible.
	-- The parameter "schematic" tells whether to convert a schematic or a component library field.
		vis_in 		: in string; -- the string to be converted
		schematic	: in boolean -- set false if it is about fields in a library, true if it is about a schematic field
		-- Explanation: The visibility of fields in schematic is defined by something like "0001" or "0000".
		-- In component libraries it is defined by characters like V or I.
		)
		return et_libraries.type_text_visible is
		
		v_in_lib : type_library_field_visible;
		v_in_sch : type_schematic_field_visible;
		v_out : et_libraries.type_text_visible;
	begin
		case schematic is
			
			when true =>
				-- As the type_schematic_field_visible has letter V as workaround, we must 
				-- prepend it here to vis_in before converting to a type_schematic_field_visible:
				v_in_sch := type_schematic_field_visible'value (schematic_field_visibility_prefix & vis_in);
				case v_in_sch is
					when V0000 => v_out := et_libraries.yes; -- visible
					when V0001 => v_out := et_libraries.no;  -- invisible
				end case;

			when false =>
				v_in_lib := type_library_field_visible'value(vis_in);
				case v_in_lib is
					when V => v_out := et_libraries.yes;
					when I => v_out := et_libraries.no;
				end case;

		end case;

		return v_out;
	end to_field_visible;

	function to_appearance (line : in type_fields_of_line; schematic : in boolean) 
	-- Converts the apperance flag to type_component_appearance.
	-- The parameter "schematic" specifies whether we are dealing with a schematic
	-- or a library component.
	-- The appearance (power symbol or normal) is defined in the component library by P/N
	-- example: DEF 74LS00 IC 0 30 Y Y 4 F N
	-- In a schematic it is defined by a hash sign:
	-- example: L P3V3 #PWR07
		return et_libraries.type_component_appearance is
		
		comp_app	: et_libraries.type_component_appearance;
		lca			: type_library_component_appearance;

		function field (
			line		: in type_fields_of_line;
			position	: in positive) return string renames get_field_from_line;

		procedure invalid_appearance is
		begin
			log_indentation_reset;
			log (message_error & et_string_processing.affected_line (line) 
				 & "invalid visibility flag !", console => true);
			raise constraint_error;
		end invalid_appearance;	

		use et_libraries;

	begin -- to_appearance
		case schematic is

			when true =>
				-- If it is about a schematic component we just test if the first
				-- character of the 3rd subfield is a hash sign.
				if field (line,3) (field (line,3)'first) = schematic_component_power_symbol_prefix then
					comp_app := sch;
				else
					comp_app := sch_pcb;
				end if;
				
			when false =>
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value (field (line,10));

				-- Evaluate lca and set comp_app accordingly.
				case lca is
					when N =>
						comp_app := sch_pcb;
					when P => 
						comp_app := sch;
				end case;
		end case;
		
		return comp_app;

		exception 
			when constraint_error =>
				invalid_appearance;
				raise;
				
	end to_appearance;

	function to_alternative_representation (line : in type_fields_of_line; schematic : in boolean)
	-- Converts the kicad alternative (deMorgan) representation to the et alternative representation.
	-- In a schematic it is expressed in a line like "U 2 1 5992967A". The 3rd field is the deMorgan flag.
		return et_schematic.type_alternative_representation is

		use et_schematic;
	
		function field (line : in type_fields_of_line; pos : in positive) return string 
			renames et_string_processing.get_field_from_line;
		
		rep_in : type_alternative_representation;
		rep_out : et_schematic.type_alternative_representation;
	begin
		rep_in := type_alternative_representation'value (field (line,3));

		case rep_in is
			when alternative_representation_yes =>
				rep_out := yes;

				-- We do not support alternative representations.
				log_indentation_reset;
				log (text => message_error & "alternative representation (DeMorgan) not supported !",
					 console => true);
				raise constraint_error;
				
			when alternative_representation_no =>
				rep_out := no;
		end case;
		
		return rep_out;

		exception
			when others => 
				log (message_error & "invalid alternative representation flag !", console => true);
				raise;			
		
	end to_alternative_representation;

	function to_degrees (angle : in string) return et_coordinates.type_angle is
	-- Converts a given angle as string to type_angle.
		
		a_in  : type_angle; -- unit is tenth of degrees -3599 .. 3599
		--a_out : et_libraries.type_angle; -- unit is degrees -359.9 .. 359.9
		use et_libraries;

		-- For the conversion we need an intermediate real type
		type type_angle_real is digits 5 range -3599.0 .. 3599.0;
		a_tmp : type_angle_real;
	begin
		-- Convert given string to et_kicad.type_angle. This implies a syntax and range check.
		a_in  := type_angle'value (angle); -- -3599 .. 3599

		-- Convert given angle to a real type
		a_tmp := type_angle_real (a_in); -- -3599.0 .. 3599.0

		-- convert given angle to et_libraries.type_angle.
		--a_out := et_libraries.type_angle (a_tmp / 10.0); -- -359.9 .. 359.9

		-- return a_out;
		return et_coordinates.type_angle (a_tmp / 10.0); -- -359.9 .. 359.9

		-- CS: exception handler
	end to_degrees;

	function to_power_flag (reference : in et_libraries.type_component_reference) return 
		et_libraries.type_power_flag is
	-- If the given component reference is one that belongs to a "power flag" returns YES.
		use et_libraries;
		use type_component_prefix;
	begin
		--log (et_schematic.to_string (reference));
		if et_libraries.prefix (reference) = power_flag_prefix then
			--log ("power flag on");
			return YES;
		else
			--log ("power flag off");
			return NO;
		end if;
	end to_power_flag;
	
	procedure read_components_libraries (log_threshold : in type_log_level) is
	-- Reads components from libraries as stored in lib_dir and project libraries:
		
        use et_libraries; -- most of the following stuff is specified there
		use et_libraries.type_full_library_names;

		-- The list of full library names tells us which libraries the project requires.
		-- This is the cursor to the full library names. It points to the library name in the current module.
		project_lib_cursor : type_full_library_names.cursor;

		-- Here we keep the full library name (incl. path) like "/home/user/lib/my_lib.lib" temporarily
		-- before inserting an empty library in the library list et_libraries.component_libraries :
		lib_file_name	: et_libraries.type_full_library_name.bounded_string;

		-- This is the library cursor. It points to the library being processed (in the list et_libraries.component_libraries):
		lib_cursor		: et_libraries.type_libraries.cursor;
		lib_inserted	: boolean; -- indicates whether a library has been inserted

		-- This is the component cursor. It points to the component being processed.
		comp_cursor		: et_libraries.type_components.cursor;
		comp_inserted	: boolean; -- indicates whether a component has been inserted

		-- This is the unit cursor. It points to the unit being processed. In kicad we deal with internal units exclusively.
		unit_cursor		: et_libraries.type_units_internal.cursor;
		unit_inserted	: boolean; -- indicates whether a unit has been inserted

		
		procedure read_library (log_threshold : in type_log_level) is
			line				: type_fields_of_line; -- the line being processed

			function field (line : in type_fields_of_line; position : in positive) return string renames
				et_string_processing.get_field_from_line;
			
			-- This flag goes true once a component section is entered. It is cleared
			-- when the component section is left.
			component_entered	: boolean := false; 

			-- The subsection of a component is indicated by variable active_section:			
			type type_active_section is ( none, fields, footprints, draw);
			active_section		: type_active_section := none; 

			-- This flag is used when ports are added to an extra unit (supply symbols).
			-- It is initialzed by procedure init_temp_variables on entering a component section.
			extra_unit_available: boolean; 
			
			-- These are variables used to temporarily hold component properties before the component
			-- gets fully assembled and inserted into the component list of a particular library.
			-- These properties apply for the whole component (means for all its units):
			tmp_component_name		: type_component_generic_name.bounded_string; -- 74LS00 -- CS: rename to generic_name
			tmp_prefix				: type_component_prefix.bounded_string; -- IC -- CS: rename to prefix
			tmp_appearance			: type_component_appearance; -- CS: rename to appearance

			tmp_port_name_visible		: type_port_name_visible;
			tmp_terminal_name_visible	: type_terminal_name_visible;
			tmp_port_name_offset	: type_distance; -- CS: rename to port_name_offset
			tmp_terminal_name	: type_terminal_name.bounded_string;
			
			tmp_units_total		: type_units_total; -- see spec for range -- CS rename to units_total	
			tmp_unit_id			: type_unit_id; -- assumes 0 if all units are affected, -- see spec	-- CS rename to unit_id

			tmp_unit_swap_level	: type_unit_swap_level := unit_swap_level_default; -- CS: rename to unit_swap_level
			tmp_unit_add_level	: type_unit_add_level := type_unit_add_level'first; -- CS: rename to unit_add_level
			tmp_unit_global		: boolean := false; -- specifies if a unit harbors component wide pins (such as power supply) -- CS: rename to unit_global
			
			field_reference		: type_text (meaning => reference); -- CS: should be field_prefix as it contains just the prefix 
			field_value			: type_text (meaning => value);
			field_commissioned	: type_text (meaning => commissioned);
			field_updated		: type_text (meaning => updated);
			field_author		: type_text (meaning => author);
			field_package		: type_text (meaning => packge);
			field_datasheet		: type_text (meaning => datasheet);
			field_purpose		: type_text (meaning => purpose);
			field_partcode		: type_text (meaning => partcode);
			field_bom			: type_text (meaning => bom);

			-- "field found flags" go true once the corresponding field was detected
			-- Evaluated by procedure check_text_fields.
			field_prefix_found			: boolean := false;
			field_value_found			: boolean := false;
			field_commissioned_found	: boolean := false;
			field_updated_found			: boolean := false;
			field_author_found			: boolean := false;
			field_package_found			: boolean := false;
			field_datasheet_found		: boolean := false;
			field_purpose_found			: boolean := false;
			field_partcode_found		: boolean := false;
			field_bom_found				: boolean := false;
			
			-- temporarily used variables to store draw elements (polylines, arcs, pins, ...) 
			-- before they are added to a unit.
			tmp_draw_polyline	: type_polyline;
			tmp_draw_rectangle	: type_rectangle;
			tmp_draw_arc		: type_arc;
			tmp_draw_circle 	: type_circle;
			tmp_draw_text		: type_symbol_text;
			tmp_draw_port		: type_port;

			-- The terminal-port map of the current component is stored here temporarily.
			-- When building the package variant (there will be only the default variant)
			-- this map is used by procedure build_package_variant.
			tmp_terminal_port_map : type_terminal_port_map.map;
			
			procedure init_temp_variables is
			-- Resets "field found flags".
			begin
				-- CS: init other variables
				extra_unit_available := false;
				tmp_unit_add_level := type_unit_add_level'first;
				tmp_unit_global := false;

				field_prefix_found			:= false;
				field_value_found			:= false;
				field_commissioned_found	:= false;
				field_updated_found			:= false;
				field_author_found			:= false;
				field_package_found			:= false;
				field_datasheet_found		:= false;
				field_purpose_found			:= false;
				field_partcode_found		:= false;
				field_bom_found				:= false;

				-- clear terminal-port map for the new component
				type_terminal_port_map.clear (tmp_terminal_port_map);
			end init_temp_variables;

			function to_swap_level (swap_in : in string)
			-- Converts the kicad interchangeable flag to the et swap level.
			-- Since Kicad has only one swap level (interchangeable yes or no) 
			-- we convert to the lowest swap level available.
			-- Used when reading component libraries.	
				return type_unit_swap_level is
				
				i : type_symbol_interchangeable;
				s : type_unit_swap_level;
				--log_threshold : type_log_level := 2;
			begin
				log_indentation_up;
				
				log ("units interchangeable", level => log_threshold + 2);
				log_indentation_up;

				i := type_symbol_interchangeable'value(swap_in);
				
				case i is
					when L =>
						log ("no", level => log_threshold + 2);
						s := 0; -- no swapping allowed
					when F =>
						log ("yes", level => log_threshold + 2);
						s := 1; -- swapping allowed at this level
				end case;

				log_indentation_down;
				log_indentation_down;
				return s;
			end to_swap_level;

			function to_pin_visibile (vis_in : in string)
			-- Converts the kicad "show pin number" flag to the et type_terminal_name_visible.
			-- Used when reading component libraries.		
				return type_terminal_name_visible is

				v_in	: type_show_pin_number;
				v_out	: type_terminal_name_visible;
			begin
				log_indentation_up;
				
				log ("pin/pad names", level => log_threshold + 2);
				log_indentation_up;
				
				v_in := type_show_pin_number'value (vis_in);
				
				case v_in is 
					when Y => 
						log ("visible", level => log_threshold + 2);
						v_out := on;
						
					when N => 
						log ("invisible", level => log_threshold + 2);
						v_out := off;
				end case;

				log_indentation_down;
				log_indentation_down;
				return v_out;
				
			end to_pin_visibile;

			function to_port_visibile (vis_in : in string)
			-- Converts the kicad "show pin name" flag to the et type_port_name_visible.
			-- Used when reading component libraries.		
				return type_port_name_visible is

				v_in	: type_show_pin_name;
				v_out	: type_port_name_visible;
			begin
				log_indentation_up;
				
				log ("port names", level => log_threshold + 2);
				log_indentation_up;
				
				v_in := type_show_pin_name'value (vis_in);
				
				case v_in is 
					when Y => 
						log ("visible", level => log_threshold + 2);
						v_out := on;
					when N => 
						log ("invisible", level => log_threshold + 2);
						v_out := off;
				end case;

				log_indentation_down;
				log_indentation_down;

				return v_out;
			end to_port_visibile;

			function to_unit_name (id : in type_unit_id) return type_unit_name.bounded_string is
			-- returns the given unit id as type_unit_name
			begin
				return type_unit_name.to_bounded_string (trim (type_unit_id'image (id), left));
			end to_unit_name;

			function to_fill (fill_style : in string) return type_fill is
			-- Converts the given kicad fill style to a type_fill.
			begin
				if fill_style = library_fill_none then
					return (pattern => none, border => invisible);
					
				elsif fill_style = library_fill_foreground then
					return (pattern => solid, border => invisible);
					
				elsif fill_style = library_fill_background then
					return (pattern => solid, border => visible);

				else
					raise constraint_error; -- CS write an error message about invalid fill style
				end if;
			end to_fill;
			
			function to_polyline (line : in et_string_processing.type_fields_of_line) return type_polyline is
			-- Returns from the given fields of a line a type_polyline.
				polyline	: type_polyline;
				points		: type_points.list;
				total		: positive; -- for cross checking 

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

				-- A polyline is defined by a string like "P 3 0 1 10 0 0 100 50 70 0 N"
				-- field meaning:
				--  #2 : number of bends (incl. start and end points) (3)
				--  #3 : 0 -> common to all units, otherwise unit id it belongs to
				--  #4 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #5 : line width
				--  #6.. 7  : start point (x/y) (0/0) 
				--  #8.. 9  : bend point (x/y) (0/100)
				-- #10..11  : end point (x/y) (50/70)
				-- last field : fill style N/F/f no fill/foreground/background
			
				-- we start processing the fields from here (where the total number of points is)
				pos 		: positive := 2; 

				-- the x position of the last point of the line is here (field #10 in example above)
				end_point	: positive := positive (et_string_processing.field_count (line)) - 2;

				-- temporarily we store coordinates of a point here
				point		: type_coordinates;
				
			begin -- to_polyline

				-- read total number of points
				total := positive'value (field (line, pos));
				
				-- read line width (field #5)
				pos := 5;
				polyline.line_width := type_line_width'value (field (line, pos));

				-- From the next field (#6) on, we find the coordinates of the 
				-- start point, the bend point(s) and the end point:
				pos := 6;
				loop exit when pos > end_point;
					set_x (point, mil_to_distance (mil => field (line, pos), warn_on_negative => false)); -- set x
					set_y (point, mil_to_distance (mil => field (line, pos+1), warn_on_negative => false)); -- set y (right after the x-field)

					-- For some unknown reason, kicad saves the y position of library objects inverted.
					-- It is probably a bug. However, when importing objects we must invert y. 
					mirror (point => point, axis => x);
					
					points.append (point); -- append this point to the list of points
					pos := pos + 2; -- advance field pointer to x coordinate of next point
				end loop;

				-- read fill style from last field
				polyline.fill := to_fill (field (line, pos));				

				-- CS: log properties
				
				return polyline;
			end to_polyline;

			function to_rectangle (line : in et_string_processing.type_fields_of_line) return type_rectangle is
			-- Returns from the given fields of a line a type_rectangle.
				rectangle	: type_rectangle;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

				-- A rectangle is defined by a string like "S -40 -100 40 100 0 1 10 N"
				-- field meaning;
				-- #2..5 : start point -40/-100   end point 40/100
				-- #6 : 0 -> common to all units, otherwise unit id it belongs to
				-- #7 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				-- #8 : line width
				-- #9 : fill style N/F/f no fill/foreground/background
			begin -- to_rectangle
				set_x (rectangle.start_point, mil_to_distance (mil => field (line,2), warn_on_negative => false));
				set_y (rectangle.start_point, mil_to_distance (mil => field (line,3), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.start_point, axis => x);
				
				set_x (rectangle.end_point, mil_to_distance (mil => field (line,4), warn_on_negative => false));
				set_y (rectangle.end_point, mil_to_distance (mil => field (line,5), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.end_point, axis => x);
				
				rectangle.line_width	:= type_line_width'value (field (line,8));
				rectangle.fill			:= to_fill (field (line,9));

				-- CS: log properties
				
				return rectangle;
			end to_rectangle;

			function to_circle (line : in et_string_processing.type_fields_of_line) return type_circle is
			-- Returns from the given fields of a circle a type_circle.
				circle	: type_circle;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;
			
				-- A circle is defined by a string like "C 0 0 112 0 1 23 N"
				-- field meaning:
				--  #2..3 : center (x/y)
				--  #4 : radius
				--  #5 : 0 -> common to all units, otherwise unit id it belongs to
				--  #6 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #7 : line width (23)
				--  #8 : fill style N/F/f no fill/foreground/background
			
			begin -- to_circle
				set_x (circle.center, mil_to_distance (mil => field (line,2), warn_on_negative => false));
				set_y (circle.center, mil_to_distance (mil => field (line,3), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => circle.center, axis => x);
	
				circle.radius		:= mil_to_distance (mil => field (line,4), warn_on_negative => false);
				circle.line_width	:= type_line_width'value (field (line,7));
				circle.fill			:= to_fill (field (line,8));

				-- CS: log properties
				
				return circle;
			end to_circle;

			function to_arc (line : in et_string_processing.type_fields_of_line) return type_arc is
			-- Returns from the given fields of an arc a type_arc.
				arc		: type_arc;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

				-- An arc is defined by a string like "A 150 0 150 1800 900 0 1 33 N 0 0 150 150"
				-- NOTE: kicad bug: multiply all y values by -1
				-- field meaning:
				--  #2..3 : center (x/y) 
				--  #4 : radius (150)
				--  #5 : start angle in tenth of degrees (1800)
				--  #6 : end angle in tenth of degrees (900)
				--  #7 : 0 -> common to all units, otherwise unit id it belongs to
				--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #9 : line width 33
				-- #10 : fill style N/F/f no fill/foreground/background
				-- #11..12 : start point (x/y)
				-- #13..14 : end point (x/y)

			begin -- to_arc
				set_x (arc.center, mil_to_distance (mil => field (line,2), warn_on_negative => false));
				set_y (arc.center, mil_to_distance (mil => field (line,3), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.center, axis => x);

				arc.radius			:= mil_to_distance (mil => field (line,4), warn_on_negative => false);

				arc.start_angle		:= to_degrees (field (line,5));
				arc.end_angle		:= to_degrees (field (line,6));
				
				arc.line_width		:= type_line_width'value (field (line,9));
				arc.fill			:= to_fill (field (line,10));
				
				set_x (arc.start_point, mil_to_distance (mil => field (line,11), warn_on_negative => false));
				set_y (arc.start_point, mil_to_distance (mil => field (line,12), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.start_point, axis => x);

				set_x (arc.end_point, mil_to_distance (mil => field (line,13), warn_on_negative => false));
				set_y (arc.end_point, mil_to_distance (mil => field (line,14), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.end_point, axis => x);
				
				-- CS: log properties
				return arc;
			end to_arc;
	
			function to_text (line : in et_string_processing.type_fields_of_line) return type_symbol_text is
			-- Returns from the given fields of a text a type_symbol_text.
				text	: type_symbol_text;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

				-- A text is defined by a string like "T 0 0 300 60 0 0 0 leuchtdiode Normal 0 C C"
				-- Space characters whitin the actual text are replaced by tilde as in this example:
				-- "T 900 -100 0 60 0 1 0 gate~C Normal 0 C C"
				-- field meaning:
				--  #2 : rotation in tenth of degrees (counter clock wise), assumes only values of 0 or 900
				--  #3..4 : center (x/y)
				--  #5 : size 
				--  #6 : ? - unknown CS
				--  #7 : 0 -> common to all units, otherwise unit id it belongs to
				--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #9 : content "leuchtdiode~square"
				-- #10 : style Nomal/Italic
				-- #11 : bold on/off (0/1) 
				-- #12 : horizontal alignment left/center/right L/C/R
				-- #13 : vertical alignment top/center/bottom T/C/B

				function to_style ( style_in : in string; bold_in : in string) return type_text_style is
				-- Composes from style_in and bold_in a type_text_style
					a : type_text_style;

					procedure invalid_style is begin
						log_indentation_reset;
						log (message_error & "invalid text style '" & style_in & "' !");
						raise constraint_error;
					end invalid_style;

				begin -- to_style
					if bold_in = library_text_bold_off then -- "0" -- bold disabled
						
						if style_in = text_library_style_normal then
							a := normal;
						elsif style_in = text_library_style_italic then
							a := italic;
						else
							invalid_style;
						end if;

					elsif bold_in = library_text_bold_on then -- "1" -- bold enabled

						if style_in = text_library_style_normal then
							a := bold;
						elsif style_in = text_library_style_italic then
							a := italic_bold;
						else
							invalid_style;
						end if;

					else -- "bold" flag invalid
						raise constraint_error; -- CS : write message on invaid "bold" flag
					end if;

					return a;
				end to_style;

				function to_content (text_in : in string) return type_text_content.bounded_string is
				-- Replaces tildss in given string by space and returns a type_text_content.bounded_string.
					t : string (1..text_in'length) := text_in; -- copy given text to t
				begin
					-- replace tildes in given text by spaces.
					translate (t, et_string_processing.tilde_to_space'access);
					return type_text_content.to_bounded_string (t);
				end to_content;
				
			begin -- to_text
				text.orientation	:= to_degrees (field (line,2));
				if text.orientation not in type_angle_90 then
					warning_angle_greater_90_degrees;
				end if;
				
				set_x (text.position, mil_to_distance (mil => field (line,3), warn_on_negative => false));
				set_y (text.position, mil_to_distance (mil => field (line,4), warn_on_negative => false));

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => text.position, axis => x);

				text.size := mil_to_distance (mil => field (line,5), warn_on_negative => false);

				-- compose from fields 10 and 11 the text style
				text.style := to_style (field (line,10), field (line,11));

				-- compose alignment
				text.alignment.horizontal	:= to_alignment_horizontal (field (line,12));
				text.alignment.vertical		:= to_alignment_vertical (field (line,13));

				-- read text content and replace tildes by spaces
				text.content				:= to_content (field (line,9));

				-- CS: log properties
				return text;
			end to_text;

			function to_port (line : in et_string_processing.type_fields_of_line) return type_port is
			-- Converts the given line to a type_port.
				use et_configuration;
			
				port : type_port; -- the port being built

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

				-- A port is defined by a string like "X ~ 1 0 150 52 D 51 50 1 1 P"
				-- field meaning:
				--  #2 : port name (~)
				--  #3 : terminal name (1)
				--  #4..5 : position x/y (0/150)
				--  #6 : port length (52)
				--  #7 : orientation up/down/left/right (U/D/L/R)
				--  #8 : terminal name size (51)
				--  #9 : port name size (50)
				-- #10 : 0 -> common to all units, otherwise unit id it belongs to
				-- #11 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				-- #12 : electrical type (direction), see et_kicad.ads for more
				-- #13 : optional field: pin style, see et_kicad.ads for more

				function to_direction (dir : in string) return type_port_direction is
					d_in : character := dir (dir'first);
					d_out : type_port_direction;
				begin
					case d_in is
						when library_pin_electrical_type_passive => 
							d_out := PASSIVE;

						when library_pin_electrical_type_input => 
							d_out := INPUT;

						when library_pin_electrical_type_output => 
							d_out := OUTPUT;

						when library_pin_electrical_type_bidir => 
							d_out := BIDIR;

						when library_pin_electrical_type_tristate => 
							d_out := TRISTATE;

						when library_pin_electrical_type_unspecified => 
							d_out := UNKNOWN;

						when library_pin_electrical_type_power_in => 
							d_out := POWER_IN;

						when library_pin_electrical_type_power_out => 
							d_out := POWER_OUT;

						when library_pin_electrical_type_open_collector => 
							d_out := WEAK1;

						when library_pin_electrical_type_open_emitter => 
							d_out := WEAK0;

						when library_pin_electrical_type_not_connected => 
							d_out := NOT_CONNECTED;

						when others =>
							-- CS: write message
							raise constraint_error;
					end case;

					return d_out;
				end to_direction;

				function to_style (style : in string) return type_port_style is
					s_in  : type_library_pin_graphical_style;
					s_out : type_port_style := type_port_style'first;
				begin
					-- convert given string to type_library_pin_graphical_style.
					-- This is an indirect test whether the given style is allowed.
					s_in := type_library_pin_graphical_style'value (style);

					-- map the given style to et.type_port_style
					case s_in is
						when N		=> s_out :=	type_port_style'first; -- default
						when I		=> s_out :=	INVERTED;
						when C		=> s_out :=	CLOCK;
						when IC		=> s_out :=	INVERTED_CLOCK;
						when L		=> s_out :=	INPUT_LOW;
						when CL		=> s_out :=	CLOCK_LOW;
						when V		=> s_out :=	OUTPUT_LOW;
						when F		=> s_out :=	FALLING_EDGE_CLK;
						when X		=> s_out :=	NON_LOGIC;
						when NI		=> s_out :=	INVISIBLE_INVERTED;
						when NC		=> s_out :=	INVISIBLE_CLOCK;
						when NIC	=> s_out :=	INVISIBLE_INVERTED_CLOCK;
						when NL		=> s_out :=	INVISIBLE_INPUT_LOW;
						when NCL	=> s_out :=	INVISIBLE_CLOCK_LOW;
						when NV		=> s_out :=	INVISIBLE_OUTPUT_LOW;
						when NF		=> s_out :=	INVISIBLE_FALLING_EDGE_CLK;
						when NX		=> s_out :=	INVISIBLE_NON_LOGIC;
					end case;
					
					return s_out;
					-- CS: exception handler
				end to_style;
				
			begin -- to_port
				log_indentation_up;

				-- port name. to be taken from field #2 of the given line
				port.name := type_port_name.to_bounded_string (field (line,2)); -- GND, GPIO2
				
				-- compose terminal name. must be stored temporarily. will be inserted in default package variant
				tmp_terminal_name := type_terminal_name.to_bounded_string (field (line,3)); -- H5, 14

				-- compose position
				set_x (port.coordinates, mil_to_distance (mil => field (line,4), warn_on_negative => false));
				set_y (port.coordinates, mil_to_distance (mil => field (line,5), warn_on_negative => false));
				mirror (point => port.coordinates, axis => x);

				-- compose length
				port.length := mil_to_distance (mil => field (line,6), warn_on_negative => false);

				-- compose orientation
				-- CS: port.orientation	:= type_library_pin_orientation

				-- port and termnal name text size
				port.terminal_name_size := mil_to_distance (mil => field (line,8), warn_on_negative => false);
				check_schematic_text_size (category => TERMINAL_NAME, size => port.terminal_name_size);

				port.port_name_size	:= mil_to_distance (mil => field (line,9), warn_on_negative => false);
				check_schematic_text_size (category => PORT_NAME, size => port.port_name_size);

				-- direction
				port.direction := to_direction (field (line,12));

				-- port style (optional, to be composed if field #13 present)
				if field_count (line) = 13 then
					port.style := to_style (field (line,13));
				end if;

				-- visibility port and pin names
				port.port_name_visible	:= tmp_port_name_visible;
				port.terminal_visible	:= tmp_terminal_name_visible;

				-- port name offset
				port.port_name_offset	:= tmp_port_name_offset;

				--log (text => et_coordinates.to_string (point => port.coordinates), level => 1);

				-- CS: log other port properties

				log_indentation_down;
				return port;

				-- CS: exception handler
			end to_port;

					
			function to_field (
				line 	: in type_fields_of_line;
				meaning	: in type_text_meaning) 
				return type_text is
			-- Reads general text field properties from subfields 3..9 and returns a type_text with 
			-- the meaning as given in parameter "meaning".
			-- Checks basic properties of text fields (allowed charactes, text size, aligment, ...)
			-- NOTE: The contextual validation takes place in procedure check_text_fields.
				use et_coordinates;
				use et_libraries.type_text_content;

				-- instantiate a text field as speficied by given parameter meaning
				text : type_text (meaning);

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;
			
			begin -- to_field
				-- field #:
				-- 3/4 : x/y coordinates
				-- 5 : size
				-- 6 : orientation (H/V)
				-- 7 : visible/invisible (V/I)
				-- 8 : aligment horizontal (R,C,L)
				-- 9 : aligment vertical (TNN, CNN, BNN) / font normal, italic, bold, bold_italic (TBI, TBN)

				check_text_content_length (strip_quotes (field (line,2)));
				text.content := type_text_content.to_bounded_string (strip_quotes (field (line,2)));
					
				-- check content vs. meaning. 
				case meaning is

					when REFERENCE =>
						check_prefix_length (content (text));
						check_prefix_characters (
							prefix => type_component_prefix.to_bounded_string (content (text)),
							characters => et_kicad.component_prefix_characters);
					
					when VALUE =>
						check_value_length (content (text));
						check_value_characters (
							value => type_component_value.to_bounded_string (content (text)),
							characters => component_value_characters);
					
					when BOM =>
						-- NOTE: length check already included in check_bom_characters
						check_bom_characters (content (text));

					when DATASHEET =>
						check_datasheet_length (content (text));
						check_datasheet_characters (
							datasheet => type_component_datasheet.to_bounded_string (content (text)));
						
					when PACKGE =>
						check_package_name_length (content (text));
						check_package_name_characters (
							packge => type_component_package_name.to_bounded_string (content (text)),
							characters => et_kicad.component_package_name_characters);

					when PURPOSE =>
						check_purpose_length (content (text));
						check_purpose_characters (
							purpose => type_component_purpose.to_bounded_string (content (text)),
							characters => component_initial_field_characters);

					when PARTCODE =>
						check_partcode_length (content (text));
						check_partcode_characters (
							partcode => type_component_partcode.to_bounded_string (content (text)),
							characters => component_initial_field_characters);

					when COMMISSIONED | UPDATED =>
						check_date_length (content (text));
						check_date_characters (
							date => type_component_date (content (text)));

					when AUTHOR =>
						check_author_length (content (text));
						check_author_characters (
							author => type_component_author.to_bounded_string (content (text)));
						
					when others => null; -- CS

				end case;
				
				set_x (text.position, mil_to_distance (mil => field (line,3), warn_on_negative => false));
				set_y (text.position, mil_to_distance (mil => field (line,4), warn_on_negative => false));
				text.size := mil_to_distance (mil => field (line,5), warn_on_negative => false);

				text.orientation := to_field_orientation (field  (line,6));
				
				text.visible := to_field_visible (
					vis_in		=> field  (line,7),
					schematic	=> false);
			
				text.alignment.horizontal := to_alignment_horizontal (field (line,8));

				text.alignment.vertical   := to_alignment_vertical (field (line,9));

				text.style := to_text_style (style_in => field (line,9), text => false);
				
				-- NOTE: text.line_width assumes default as no explicit line width is provided here.
				return text;

			end to_field;

			procedure check_text_fields (log_threshold : in type_log_level) is
			-- Tests if all text fields have been found by evaluating the "field found flags".
			-- Validates the fields in CONTEXT WITH EACH OTHER.
			-- NOTE: This is library related stuff.

				-- CS: check orientation.
				-- CS: check visibility.
				-- CS: check hor aligment.
				-- CS: check vert aligment.
				-- CS: check style.

			
				procedure missing_field (meaning : in et_libraries.type_text_meaning) is 
				begin
					log_indentation_reset;
					log (message_error & "text field " & to_string (meaning) & " missing !",
						console => true);
					raise constraint_error;
				end missing_field;

				use et_configuration;
				
			begin -- check_text_fields
				log_indentation_up;

				-- write precheck preamble
				log ("component " & to_string (tmp_component_name) & " prechecking fields ...", level => log_threshold);
				log_indentation_up;

				log ("value", level => log_threshold + 1);
				if not field_value_found then
					missing_field (field_value.meaning);
				else
					-- KiCad insists that the value contains something.
					-- So the first choice is to set value like the generic component name:
					if content (field_value) /= to_string (strip_tilde (tmp_component_name)) then
						log (message_warning & "default value " 
							& content (field_value) & " differs from name "
							& to_string (tmp_component_name) & " !");
					end if;

					check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_value.size);
				end if;
				
				log ("author", level => log_threshold + 1);				
				if not field_author_found then
					missing_field (field_author.meaning);
				else
					check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_author.size);
					null; -- CS validate_author
				end if;

				log ("commissioned", level => log_threshold + 1);
				if not field_commissioned_found then
					missing_field (field_commissioned.meaning);
				else
					check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_commissioned.size);
					null; -- CS validate_commissioned
				end if;

				log ("updated", level => log_threshold + 1);
				if not field_updated_found then
					missing_field (field_updated.meaning);
				else
					-- The update must be later than the commission date:
					if compare_date (
						left => type_component_date (content (field_updated)),
						right => type_component_date (content (field_commissioned))) then
						log (message_warning & "commission date must be before update !");
						-- CS: show reference, commission and update time
					end if;

					check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_updated.size);
				end if;
				
				-- appearance specific fields:
				case tmp_appearance is
					when sch_pcb =>
						-- This is a real component.

						-- Since this is a real component. we do the prefix character check 
						-- against the default character set for prefixes as specified in et_libraries.
						-- Afterward we validate the prefix. The prefixes for real components are specified
						-- in the et configuration file (see et_configuration).						
						log ("prefix", level => log_threshold + 1);
						if not field_prefix_found then
							missing_field (field_reference.meaning);
						else
							check_prefix_characters (
								prefix => tmp_prefix,
								characters => et_libraries.component_prefix_characters);
							
							et_configuration.validate_prefix (tmp_prefix);
							
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
						end if;

						log ("package/footprint", level => log_threshold + 1);
						if not field_package_found then
							missing_field (field_package.meaning);
						else
							validate_component_package_name (
								type_component_package_name.to_bounded_string (field (
									line => read_line ( -- CS use function package_name
										line => content (field_package), -- bel_ic:S_SO14
										ifs => latin_1.colon),
									position => 2))); -- the block after the colon

							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_package.size);
						end if;
						
						log ("partcode", level => log_threshold + 1);
						if not field_partcode_found then
							missing_field (field_partcode.meaning);
						else
							log_indentation_up;
							
							validate_component_partcode_in_library (
								-- the content of the partcode field like R_PAC_S_0805_VAL_
								partcode => type_component_partcode.to_bounded_string (content (field_partcode)),

								name => tmp_component_name, -- 74LS00
								
								-- the component prefix like LED
								prefix => tmp_prefix,

								-- The component package name like S_0805 must be extracted from the field text_package.
								-- The field contains something like bel_ic:S_SO14 . 
								-- The part after the colon is the package name. The part before the colon is the library
								-- name which is not of interest here.
								packge => type_component_package_name.to_bounded_string (field (
									line => read_line ( -- CS use function package_name
										line => content (field_package), -- bel_ic:S_SO14
										ifs => latin_1.colon),
									position => 2)), -- the block after the colon

								-- the BOM status
								bom => type_bom'value (content (field_bom)),

								log_threshold => log_threshold + 1
								);
							
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_partcode.size);

							log_indentation_down;
						end if;

						log ("datasheet", level => log_threshold + 1);
						if not field_datasheet_found then
							missing_field (field_datasheet.meaning);
						else
							-- CS validate_datasheet
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_datasheet.size);
						end if;

						log ("bom", level => log_threshold + 1);
						if not field_bom_found then
							missing_field (field_bom.meaning);
						else
							-- CS validate_bom_status
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_bom.size);
						end if;
						
						log ("purpose", level => log_threshold + 1);
						if not field_purpose_found then
							missing_field (field_purpose.meaning);
						else
							-- we do not expect a dedicated purpose here but only the purpose_default string
							if content (field_purpose) /= purpose_default then
								log (message_warning & "expected default " & purpose_default & " !");
							end if;

							-- If component requires user interaction,
							-- make sure the purpose text is visible in the graphical representation:
							if requires_operator_interaction (tmp_prefix) = YES then
								if field_purpose.visible = no then
									log_indentation_reset;
									log (message_error & "component "
										& to_string (tmp_component_name)
										& " purpose not visible !",
										console => true);
									raise constraint_error;
								end if;
							end if;

							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_purpose.size);
						end if;

						
					when sch =>
						-- Since this is a virtual component, we do the prefix character check
						-- against the Kicad specific character set for prefixes. see et_kicad.ads.
						-- Afterward we validate the prefix. The prefixes for virtual components
						-- are KiCad specific.
						log ("prefix", level => log_threshold + 1);
						if not field_prefix_found then
							missing_field (field_reference.meaning);
						else
							check_prefix_characters (
								prefix => tmp_prefix,
								characters => component_prefix_characters);
							
							validate_prefix (tmp_prefix);

							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
						end if;

					when pcb => null; --CS
						
				end case;
				
				log_indentation_down;
				log_indentation_down;				

			end check_text_fields;

			
			procedure insert_component (
			-- NOTE: This is library related stuff.
			-- Updates the current library by inserting the component.
			-- If the component was inserted (should be) the comp_cursor points to the component
			-- for later inserting the units:
				key			: in type_full_library_name.bounded_string;
				components	: in out type_components.map) is
			begin -- insert_component

-- 				-- For the logfile write the component name.
-- 				-- If the component contains more than one unit, write number of units.

				check_text_fields (log_threshold + 2);

				case tmp_appearance is
					when sch =>

						-- we insert into the given components list a new component
						type_components.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like #PWR, #FLG 
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> sch,

								-- Whether the component is a power flag can be reasoned by the prefix.
								-- At library level there is no indexed prefix. Power flags have just 
								-- the prefix "#FLG". So we can provide an arbitrary index for the conversion
								-- function "to_power_flag".
								power_flag		=> to_power_flag (et_schematic.to_component_reference (
														text_in => to_string (tmp_prefix) & "0", -- #FLG0
														allow_special_character_in_prefix => true)), -- because of the '#'

								prefix			=> tmp_prefix,
								value			=> type_component_value.to_bounded_string (content (field_value)),
								commissioned	=> type_component_date (content (field_commissioned)),
								updated			=> type_component_date (content (field_updated)),
								author			=> type_person_name.to_bounded_string (content (field_author)),
								units_internal	=> type_units_internal.empty_map,
								units_external	=> type_units_external.empty_map
								)
							);
						
					when sch_pcb =>

						-- we insert into the given components list a new component
						type_components.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like 74LS00
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> sch_pcb,
								prefix			=> tmp_prefix,
								value			=> type_component_value.to_bounded_string (content (field_value)),
								commissioned	=> type_component_date (content (field_commissioned)),
								updated			=> type_component_date (content (field_updated)),
								author			=> type_person_name.to_bounded_string (content (field_author)),
								units_internal	=> type_units_internal.empty_map,
								units_external	=> type_units_external.empty_map,

								package_filter	=> type_package_filter.empty_set,
								datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),
								purpose			=> type_component_purpose.to_bounded_string (content (field_purpose)),

								partcode		=> type_component_partcode.to_bounded_string (content (field_partcode)),

								bom				=> type_bom'value (content (field_bom)),

								variants		=> type_component_variants.empty_map
								)
							);


					when others =>
						null; -- CS -- should not happen at all
						raise constraint_error;
						
				end case;

				-- Raise alarm if compoenent is already in the libaray.
				if comp_inserted then
					null;
				else
					log_indentation_reset;
					log (text => message_error & "line" & affected_line (line) & " : component already in library !",
						 console => true);
					raise constraint_error;
				end if;

				exception
					when constraint_error =>
						log_indentation_reset;
						log (text => message_error & "component " & to_string (tmp_component_name) & " invalid !",
							 console => true);
							-- CS: provide details about the problem (line number, ...)
						raise;
						
			end insert_component;

			
			procedure set_unit_cursor (libraries : in out type_libraries.map) is
			-- Sets the unit_cursor according to the current unit_id.
			-- If the unit_id is 0, the unit_cursor is not changed.
		
				procedure locate_unit (
				-- sets the unit_cursor
					key			: in type_component_generic_name.bounded_string;
					component	: in type_component) is
				begin
					unit_cursor := component.units_internal.find (to_unit_name (tmp_unit_id));
				end locate_unit;

				procedure locate_component ( 
					key			: in type_full_library_name.bounded_string;
					components	: in type_components.map) is
				begin
					type_components.query_element (comp_cursor, locate_unit'access);
				end locate_component;

			begin -- set_unit_cursor
				if tmp_unit_id > 0 then -- if tmp_unit_id is zero, nothing is done
					type_libraries.query_element (lib_cursor, locate_component'access);
				end if;
			end set_unit_cursor;
				
			
			procedure add_unit (libraries : in out type_libraries.map) is -- CS: no need for parameter libraries. use component_libraries directly on update
			-- Add the unit with current tmp_unit_id to current component (indicated by comp_cursor).
			-- Leaves unit_cursor pointing to unit that has been added.
			
			-- If the unit has already been inserted, nothing happens.
			
			-- If the tmp_unit_id is 0 and the total number of units is 1, tmp_unit_id is set to 1
			-- and a single unit is added. This is the case if the component has only one unit
			-- and the draw object has the check "common to all units" set.
			
			-- If the tmp_unit_id is 0 and the total number of units is greater 1, nothing happens. 
			-- This is the case when the component has more than one unit and the draw object
			-- has the check "common to all units" set.

			-- The current tmp_unit_swap_level determines the swap level of the unit to be inserted.
			-- The current tmp_unit_add_level determines the add level of the unit to be inserted. It is "request"
			-- in case the unit in question is an extra unit (supply unit).
			
				procedure insert_unit (
				-- Inserts an internal unit in a component.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component) is

					unit : type_unit_internal (tmp_appearance);
				begin
					unit.swap_level	:= tmp_unit_swap_level;
					unit.add_level	:= tmp_unit_add_level;
					unit.global		:= tmp_unit_global;
					
					component.units_internal.insert (
						key			=> to_unit_name (tmp_unit_id),
						new_item	=> unit,
						position	=> unit_cursor,
						inserted	=> unit_inserted);
				end insert_unit;

				procedure locate_component ( 
					key			: in type_full_library_name.bounded_string;
					components	: in out type_components.map) is
				begin
					components.update_element (comp_cursor, insert_unit'access);
				end locate_component;

			begin -- add_unit
				if tmp_unit_id > 0 then
					libraries.update_element ( lib_cursor, locate_component'access);
				elsif tmp_units_total = 1 then
					tmp_unit_id := 1;
					libraries.update_element ( lib_cursor, locate_component'access);
				else
					null; -- CS
				end if;
			end add_unit;

			procedure create_units is
			-- Creates empty units in the current component.
			-- The number of units is set by unit_total 
			-- (earlier derived from the component header like "DEF 74LS00 IC 0 30 Y Y 4 F N")
			begin
				for u in 1 .. type_unit_id (tmp_units_total) loop
					tmp_unit_id := u;
					add_unit (component_libraries);
				end loop;
			end create_units;

				
			procedure add_symbol_element (
			-- Adds a symbol element (circle, arcs, lines, ports, etc.) to the unit with the current tmp_unit_id.
			-- The kind of symbol element is given by parameter "element".
			-- The symbol properties are taken from the temporarily variables named tmp_draw_*.
						
				libraries	: in out type_libraries.map; -- CS: no need for parameter libraries. use component_libraries directly on update
				element		: in type_symbol_element) is

				procedure insert (
				-- Inserts the given element in the unit.
				-- If a port is to be inserted: Aborts on multiple usage of port or pin names.
					key		: in type_unit_name.bounded_string;
					unit	: in out type_unit_internal) is
					pos		: natural := 0; -- helps to trace the program position where an exception occured
				begin
					case element is
						when polyline =>
							unit.symbol.shapes.polylines.append (tmp_draw_polyline);

						when rectangle =>
							unit.symbol.shapes.rectangles.append (tmp_draw_rectangle);

						when arc =>
							unit.symbol.shapes.arcs.append (tmp_draw_arc);
							
						when circle =>
							unit.symbol.shapes.circles.append (tmp_draw_circle);

						when text =>
							unit.symbol.texts.append (tmp_draw_text);

						when port =>
							pos := 100;
							-- CS: test if port not used by other units
							pos := 110;
							-- CS: test if pin name not used by other units
							pos := 190;
							unit.symbol.ports.append (tmp_draw_port);

							type_terminal_port_map.insert (
								container => tmp_terminal_port_map,
								key => tmp_terminal_name, -- terminal name
								new_item => (
									name => tmp_draw_port.name, -- port name
									unit => to_unit_name (tmp_unit_id))); -- unit name
							
							
						when others =>
							raise constraint_error;
					end case;

-- 					exception 
-- 						when constraint_error =>
-- 							case pos is
-- 								when 190 => 
-- 									-- Tell the operator which port name the problem is:
-- 									log_indentation_reset;
-- 									log (
-- 										text => message_error & "file '" 
-- 											& et_libraries.to_string (lib_file_name) & "' "
-- 											& affected_line (line) 
-- 											& "port name '" & to_string (tmp_draw_port_name)
-- 											& "' already used !",
-- 										console => true);
-- 									raise;
-- 									
-- 								when others =>
-- 									raise;
-- 							end case;
-- 							
-- 						when others => raise;
				end insert;
				
				procedure locate_unit (
				-- Locates the unit indicated by unit_cursor.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component) is
				begin
					component.units_internal.update_element (unit_cursor, insert'access);
				end locate_unit;
				
				procedure locate_component ( 
				-- Locates the component indicated by comp_cursor.
					key			: in type_full_library_name.bounded_string;
					components	: in out type_components.map) is
				begin -- locate_component
					components.update_element (comp_cursor, locate_unit'access);
				end locate_component;

			begin -- add_symbol_element
				if tmp_unit_id > 0 then 
					--log ("unit id " & type_unit_id'image (tmp_unit_id) , level => 1);
					-- The element belongs to a particular unit exclusively.
					-- Only the current unit of the current component receives the symbol element.
					set_unit_cursor (component_libraries); -- set unit_cursor according to current tmp_unit_id
					libraries.update_element (lib_cursor, locate_component'access);
				else -- tmp_unit_id = 0
					-- The element belongs to all units of the current component.
					-- In a loop the tmp_unit_id is now modified so that all units (except extra units) 
					-- of the component receive the same symbol element.
					-- units_total was set on passing the component header (DEF 74LS00 IC 0 30 Y Y 4 F N)
					if element /= port then -- should always be true since tmp_unit_id is always greater zero when a port is added to a unit
						for u in 1 .. type_unit_id (tmp_units_total) loop
							tmp_unit_id := u; -- set tmp_unit_id
							set_unit_cursor (component_libraries);  -- set unit_cursor according to current tmp_unit_id
							libraries.update_element (lib_cursor, locate_component'access);
						end loop;
					else
						raise constraint_error; -- should never happen. see comment above after "if" statement
					end if;
				end if;
			end add_symbol_element;
			

			procedure set_text_placeholder_properties (libraries : in out type_libraries.map) is -- CS: no need for parameter libraries. use component_libraries directly on update
			-- Sets the properties of placeholders in all units of the component indicated by comp_cursor.
			
				procedure set (
				-- Sets the properties of the placeholders in the current unit.
					key		: in type_unit_name.bounded_string;
					unit	: in out type_unit_internal) is
				begin
					-- For the unit we are interested in the properties of the component text fields.
					-- The component text fields as given in the component section look like "F0 "IC" 0 50 50 H V C BIB".
					-- The content (in this example "IC") is not relevant here as it applies for the whole component.
					-- We convert the text field downward to a type_text_basic (which strips off the content) first.
					-- Then we convert the type_text_basic upward to type_text_placeholder by providing the meaning:
					unit.symbol.reference	:= (type_text_basic (field_reference)		with meaning => reference);
					unit.symbol.value		:= (type_text_basic (field_value)			with meaning => value);
					unit.symbol.commissioned:= (type_text_basic (field_commissioned)	with meaning => commissioned);
					unit.symbol.updated		:= (type_text_basic (field_updated)			with meaning => updated);
					unit.symbol.author		:= (type_text_basic (field_author)			with meaning => author);

					case unit.symbol.appearance is
						when sch_pcb =>
							unit.symbol.packge		:= (type_text_basic (field_package)		with meaning => packge);
							unit.symbol.datasheet	:= (type_text_basic (field_datasheet)	with meaning => datasheet);
							unit.symbol.purpose		:= (type_text_basic (field_purpose)		with meaning => purpose);
							unit.symbol.partcode	:= (type_text_basic (field_partcode)	with meaning => partcode);
							unit.symbol.bom			:= (type_text_basic (field_bom)			with meaning => bom);
						when others => null;
					end case;
				end set;
				
				procedure locate_unit (
				-- Locates the unit indicated by unit_cursor.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component) is
				begin
					component.units_internal.update_element (unit_cursor, set'access);
				end locate_unit;
				
				procedure locate_component ( 
				-- Locates the component indicated by comp_cursor.
					key			: in type_full_library_name.bounded_string;
					components	: in out type_components.map) is
				begin -- locate_component
					components.update_element (comp_cursor, locate_unit'access);
				end locate_component;

				-- Set the number of units to be updated:
				total : type_unit_id := type_unit_id (tmp_units_total);
				
			begin -- set_text_placeholder_properties

				-- The number of units to be updated increases by one if an extra unit has been created:
				if extra_unit_available then total := total + 1; end if;
				
				-- In a loop we set tmp_unit_id for each unit and update the unit of the component.
				for u in 1..total loop
					tmp_unit_id := u;
					set_unit_cursor (component_libraries);
					libraries.update_element (lib_cursor, locate_component'access);
				end loop;
				
			end set_text_placeholder_properties;
			
			procedure read_draw_object (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- Creates a symbol element from the given line and adds it to the unit indicated by tmp_unit_id.
				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;
					
				function to_unit_id (text : in string) return type_unit_id is
				-- converts a unit id given as string to type_unit_id
				begin 
					return type_unit_id'value (text);
					-- CS: exception handler
				end to_unit_id;

				procedure write_scope_of_object (unit : in type_unit_id) is
				-- Outputs whether the current draw object is common to all units or not.
				begin
					log_indentation_up;
				
					if unit = 0 then
						log ("scope: common to all units", log_threshold + 1);
					else
						log ("scope: unit" & type_unit_id'image (unit), log_threshold + 1);
					end if;
					
					log_indentation_down;
				end write_scope_of_object;

				draw_object : constant string (1..12) := "draw object ";
				
			begin -- read_draw_object
				--log ("draw object", level => log_threshold + 1);
				--log_indentation_up;

				-- At a certain log level we report the bare line of a draw object as it is:
				--log (to_string (line), log_threshold + 2);
				
				case type_library_draw'value (field (line,1)) is
					when P => -- polyline
						log (draw_object & "polyline", log_threshold);
						-- A polyline is defined by a string like "P 3 0 1 10 0 0 100 50 70 0 N"
						-- field meaning:
						--  #2 : number of bends (incl. start and end points) (3)
						--  #3 : 0 -> common to all units, otherwise unit id it belongs to
						--  #4 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #5 : line width
						--  #6.. 7  : start point (x/y) (0/0) 
						--  #8.. 9  : bend point (x/y) (0/100)
						-- #10..11  : end point (x/y) (50/70)
						-- last field : fill style N/F/f no fill/foreground/background

						log (to_string (line), log_threshold + 1);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,3));
						write_scope_of_object (tmp_unit_id);

						-- compose polyline
						tmp_draw_polyline := to_polyline (line);

						-- add polyline to unit
						add_symbol_element (component_libraries, polyline);
						
					when S => -- rectangle
						log (draw_object & "rectangle", log_threshold);
						-- A rectangle is defined by a string like "S -40 -100 40 100 0 1 10 N"
						-- field meaning;
						-- #2..5 : start point -40/-100   end point 40/100
						-- #6 : 0 -> common to all units, otherwise unit id it belongs to
						-- #7 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						-- #8 : line width
						-- #9 : fill style N/F/f no fill/foreground/background

						log (to_string (line), log_threshold + 1);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,6));
						write_scope_of_object (tmp_unit_id);

						-- compose rectangle
						tmp_draw_rectangle := to_rectangle (line);

						-- add rectangle to unit
						add_symbol_element (component_libraries, rectangle);
						
					when C => -- circle
						log (draw_object & "circle", log_threshold);
						-- A circle is defined by a string like "C 0 0 112 0 1 23 N"
						-- field meaning:
						--  #2..3 : center (x/y)
						--  #4 : radius
						--  #5 : 0 -> common to all units, otherwise unit id it belongs to
						--  #6 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #7 : line width (23)
						--  #8 : fill style N/F/f no fill/foreground/background

						log (to_string (line), log_threshold + 1);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,5));
						write_scope_of_object (tmp_unit_id);

						-- compose circle
						tmp_draw_circle := to_circle (line);
						
						-- add circle to unit
						add_symbol_element (component_libraries, circle);
						
					when A => -- arc
						log (draw_object & "arc", log_threshold);
						-- An arc is defined by a string like "A 150 0 150 1800 900 0 1 33 N 0 0 150 150"
						-- NOTE: kicad bug: multiply all y values by -1
						-- field meaning:
						--  #2..3 : center (x/y) 
						--  #4 : radius (150)
						--  #5 : start angle in tenth of degrees (1800)
						--  #6 : end angle in tenth of degrees (900)
						--  #7 : 0 -> common to all units, otherwise unit id it belongs to
						--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #9 : line width 33
						-- #10 : fill style N/F/f no fill/foreground/background
						-- #11..12 : start point (x/y)
						-- #13..14 : end point (x/y)

						log (to_string (line), log_threshold + 1);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,7));
						write_scope_of_object (tmp_unit_id);

						-- compose arc
						tmp_draw_arc := to_arc (line);
						
						-- add arc to unit
						add_symbol_element (component_libraries, arc);

					when T => -- text
						log (draw_object & "text", log_threshold);
						-- A text is defined by a string like "T 0 0 300 60 0 0 0 leuchtdiode Normal 0 C C"
						-- Space characters whitin the actual text are replaced by tilde as in this example:
						-- "T 0 -100 0 60 0 1 0 gate~C Normal 0 C C"
						-- field meaning:
						--  #2 : rotation in tenth of degrees (counter clock wise), assumes only values of 0 or 900
						--  #3..4 : center (x/y)
						--  #5 : size 
						--  #6 : ? - unknown CS
						--  #7 : 0 -> common to all units, otherwise unit id it belongs to
						--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #9 : content "leuchtdiode"
						-- #10 : style Nomal/Italic
						-- #11 : bold on/off (0/1) 
						-- #12 : alignment left/center/right L/C/R
						-- #13 : alignment top/center/bottom T/C/B

						log (to_string (line), log_threshold + 1);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,7));
						write_scope_of_object (tmp_unit_id);

						-- compose text
						tmp_draw_text := to_text (line);
						
						-- add text to unit
						add_symbol_element (component_libraries, text);
						
					when X => -- port
						log (draw_object & "port", log_threshold);
						-- A port is defined by a string like "X ~ 1 0 150 52 D 51 50 1 1 P"
						-- field meaning:
						--  #2 : port name (~)
						--  #3 : pin number (1)
						--  #4..5 : position x/y (0/150)
						--  #6 : pin length (52)
						--  #7 : orientation up/down/left/right (U/D/L/R)
						--  #8 : pin number size (51)
						--  #9 : port name size (50)
						-- #10 : 0 -> common to all units, otherwise unit id it belongs to
						-- #11 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						-- #12 : electrical type (direction), see et_kicad.ads for more
						-- #13 : optional field: pin style, see et_kicad.ads for more

						log (to_string (line), log_threshold + 1);
						-- CS: output properties in a human readable form instead.
						
						tmp_unit_id := to_unit_id (field (line,10));
						write_scope_of_object (tmp_unit_id);

						-- compose port
						tmp_draw_port := to_port (line);

						-- If this is a unit specific port it gets added to the unit. If it applies for the
						-- whole component, we create an extra unit and insert it there. An extra unit is
						-- created ONLY ONCE. Successive unit-wide ports are added there.
						-- An extra unit always has the add level "request" since it harbors the supply ports.
						-- When adding a port, tmp_unit_id is always greater zero.
						if tmp_unit_id > 0 then
							-- add unit specific port to unit
							--log ("unit id " & type_unit_id'image (tmp_unit_id) , level => log_threshold);
							add_symbol_element (libraries => component_libraries, element => port);
						else 
							-- The unit id changes from 0 to tmp_units_total + 1 (one notch above the total number) :
							tmp_unit_id := type_unit_id (tmp_units_total) + 1;
							-- If no extra unit has been created yet -> create one with add level "request".
							if not extra_unit_available then 
								tmp_unit_add_level := request;
								tmp_unit_global := true; -- this is a unit with power supply pins that apply for the whole component
								add_unit (component_libraries);
								extra_unit_available := true;
							else
								null;
							end if;
							-- insert the port in the extra unit
							--log ("unit id " & type_unit_id'image (tmp_unit_id) , level => log_threshold);						
							add_symbol_element (libraries => component_libraries, element => port);
						end if;
				end case;

				--log_indentation_down;
			end read_draw_object;

			procedure add_footprint (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- Reads the proposed footprint and adds it to the package filter of the current component.
				fp : type_package_proposal.bounded_string;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;
				
				procedure do_it (libraries : in out type_libraries.map) is -- CS: no need for parameter libraries. use component_libraries directly on update
				-- Adds the footprint finally.
					procedure insert_footprint (
						key			: in type_component_generic_name.bounded_string;
						component	: in out type_component) is
					begin
						component.package_filter.insert (fp);
					end insert_footprint;
					
					procedure locate_component ( 
						key			: in type_full_library_name.bounded_string;
						components	: in out type_components.map) is
					begin
						components.update_element (comp_cursor, insert_footprint'access);
					end locate_component;

				begin -- add_footprint
					libraries.update_element (lib_cursor, locate_component'access);
				end do_it;
				
			begin -- add_footprint
	-- 			log ("footpint/package filter", level => log_threshold + 1);
				log_indentation_up;

				fp := type_package_proposal.to_bounded_string (field (line,1));
				log (type_package_proposal.to_string (fp), log_threshold);

				do_it (component_libraries);
				
				log_indentation_down;
			end add_footprint;
		

			procedure read_field (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- NOTE: This is library related stuff.
			-- Reads the text field of a component in a set of temporarily variables field_reference, field_value, ...
			-- Sets the "field found flag" according to the field being detected.
			-- Text fields look like:
			-- F0 "#PWR" 0 -200 50 H I C CNN
			-- F1 "GND" 0 -100 50 H V C CNN
			-- F2 "" 0 0 50 H I C CNN
			-- F3 "" 0 0 50 H I C CNN
			-- F4 "1974-12-27T23:04:22" 650 100 60 H I C CNN "commissioned"
			-- F5 "2017-01-23T23:04:22" 650 0 60 H I C CNN "updated"
			-- F6 "MBL" 450 -100 60 H I C CNN "author"
				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;
					
				use et_string_processing;

			begin -- read_field
				
				-- read text fields from a component library (thats why scheamtic => false)
				case to_text_meaning (line => line, schematic => false) is

					-- If we have the prefix field like "F0 "U" 0 50 50 H V C CNN"
					when reference =>
									
						-- CS: Do a cross check of prefix and reference -- "U" 
						-- The prefix is already defined in the component hearder. 
						-- Why this redundance ? Ask the kicad makers...
						if strip_quotes (field (line,2)) = type_component_prefix.to_string (tmp_prefix) then
							null; -- fine
						else
							log (message_warning & affected_line (line) & ": prefix vs. reference mismatch !");
							-- CS: better raise constraint_error
						end if;

						field_prefix_found := true;
						field_reference := to_field (line => line, meaning => reference);
						-- for the log:
						write_text_properies (type_text (field_reference), log_threshold + 1);

					-- If we have a value field like "F1 "74LS00" 0 -100 50 H V C CNN"
					when value =>
						field_value_found := true;
						field_value := to_field (line => line, meaning => value);
						
						-- for the log:
						write_text_properies (type_text (field_value), log_threshold + 1);

					-- If we have a footprint field like "F2 "bel_resistors:S_0805" 0 -100 50 H V C CNN"
					-- NOTE: the part before the colon is the containing library. The part after the colon 
					-- is the actual footprint/package name.
					when packge =>

						field_package_found := true;
						field_package := to_field (line => line, meaning => packge);
						-- for the log:
						write_text_properies (type_text (field_package), log_threshold + 1);

					-- If we have a datasheet field like "F3 "" 0 -100 50 H V C CNN"
					when datasheet =>

						field_datasheet_found := true;
						field_datasheet := to_field (line => line, meaning => datasheet);
						-- for the log:
						write_text_properies (type_text (field_datasheet), log_threshold + 1);

					-- Other mandatory fields like function and partcode are detected by F4 and F5 
					-- (not by subfield #10 !) So F4 enforces a function, F5 enforces a partcode.
					
					-- If we have a purpose field like "F9 "" 0 -100 50 H V C CNN" "purpose",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when purpose =>
					
						if to_lower (strip_quotes (field (line,10))) = to_lower (type_text_meaning'image (purpose)) then
							field_purpose_found := true;
							field_purpose := to_field (line => line, meaning => purpose);
							-- for the log:
							write_text_properies (type_text (field_purpose), log_threshold + 1);
							-- basic_text_check(fnction); -- CS
						else
							invalid_field (line);
						end if;

					-- If we have a partcode field like "F7 "" 0 -100 50 H V C CNN" "partcode",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when partcode =>
					
						if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (partcode)) then
							field_partcode_found := true;
							field_partcode := to_field (line => line, meaning => partcode);
							-- for the log:
							write_text_properies (type_text (field_partcode), log_threshold + 1);
							-- basic_text_check(partcode); -- CS
						else
							invalid_field (line);
						end if;

					-- If we have a "commissioned" field like "F4 "" 0 -100 50 H V C CNN" "commissioned",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when commissioned =>
					
						if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (commissioned)) then
							field_commissioned_found := true;
							field_commissioned := to_field (line => line, meaning => commissioned);
							-- for the log:
							write_text_properies (type_text (field_commissioned), log_threshold + 1);
							-- basic_text_check(commissioned); -- CS
						else
							invalid_field (line);
						end if;

					-- If we have an "updated" field like "F5 "" 0 -100 50 H V C CNN" "updated",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when updated =>
					
						if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (updated)) then
							field_updated_found := true;
							field_updated := to_field (line => line, meaning => updated);
							-- for the log:
							write_text_properies (type_text (field_updated), log_threshold + 1);
							-- basic_text_check(updated); -- CS
						else
							invalid_field (line);
						end if;

					-- If we have an "author" field like "F6 "" 0 -100 50 H V C CNN" "author",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when author =>
					
						if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (author)) then
							field_author_found := true;
							field_author := to_field (line => line, meaning => author);
							-- for the log:
							write_text_properies (type_text (field_author), log_threshold + 1);
							-- basic_text_check(author); -- CS
						else
							invalid_field (line);
						end if;

					-- If we have a "bom" field like "F8 "" 0 -100 50 H V C CNN" "bom",
					-- we test subfield #10 against the prescribed meaning. If ok the field is read like
					-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
					when bom =>
					
						if to_lower (strip_quotes (field (line,10))) = to_lower (type_text_meaning'image (bom)) then
							field_bom_found := true;
							field_bom := to_field (line => line, meaning => bom);
							-- for the log:
							write_text_properies (type_text (field_bom), log_threshold + 1);
							-- basic_text_check (bom); -- CS
						else
							invalid_field (line);
						end if;

						
					when others => null;
						-- CS: warning about illegal fields ?
						-- CS: other text fields ?
				end case;

				
				-- CS: check appearance vs. function vs. partcode -- see stock_manager	
			end read_field;
		

			procedure build_package_variant is
			-- Builds from tmp_terminal_port_map and field_package the default package variant.
			-- NOTE: Since kicad does not know package variants, we can only build the
			-- one and only DEFAULT variant. The default variant name is the same as the package name.
			-- All this applies for real components only (appearance sch_pcb).
				
				procedure locate_component (
					lib_name	: in type_full_library_name.bounded_string;
					components	: in out type_components.map) is

					procedure build (
						comp_name	: in type_component_generic_name.bounded_string;
						component	: in out type_component) is

						use type_component_variants;
						use type_terminal_port_map;

						tmp_variant_name : type_component_variant_name.bounded_string; -- temporarily used for building the variant name
						tmp_variants : type_component_variants.map; -- temporarily used for building the variant
					begin
						case component.appearance is
							when sch_pcb => -- real component

								-- The name of the default variant is the package 
								-- name (instead of an empty string or a string like "default"):
								check_variant_name_length (to_string (package_name (content (field_package)))); -- S_SO14
								tmp_variant_name := to_component_variant_name (to_string (package_name (content (field_package)))); -- S_SO14
								check_variant_name_characters (tmp_variant_name);

								-- Insert in tmp_variants (which is temporarily) the default variant.
								insert (
									container => tmp_variants,

									key => tmp_variant_name, -- S_SO14
									
									new_item => (
										-- The package field contains something like "bel_ic:S_SO14".
										-- This provides the library name and the package name.
										-- The only way to obtain the number of terminals is to read the
										-- length of the tmp_terminal_port_map.
										-- CS: NOTE: Since not all terminals of the package may be mapped to ports,
										-- this approach implies the risk of a wrong terminal count !
										-- Example: If a S_SO14 housing contains just a single NAND gate (with supply) the
										-- tmp_terminal_port_map would have a length of 5 whereas the package
										-- would have 14 terminals.
										-- CS: A function is required that guesses from the package name the
										-- real number of terminals.
										-- CS: Even better a function that fetches the pad count from the 
										-- package model (package et_pcb) ?
										packge => (
											name => package_name (content (field_package)), -- S_SO14

											-- We compose the full library name from lib_dir (global variable) and the 
											-- library name. example: projects/lbr/bel_ic
											library => to_full_library_name (
												root_dir => lib_dir,
												lib_name => library_name (content (field_package))), 

											-- derive terminal count from tmp_terminal_port_map
											-- CS: see comments above
											terminal_count => type_terminal_count (length (tmp_terminal_port_map))),

										-- The terminal to port map tmp_terminal_port_map is now finally copied
										-- to its final destination:
										terminal_port_map => tmp_terminal_port_map)); -- H4/GPIO2

								-- Assign package variant to component
								component.variants := tmp_variants;

							when others => null;
						end case;
					end build;
					
				begin -- locate_component
					components.update_element (comp_cursor, build'access);
				end locate_component;
				
			begin -- build_package_variant
				log_indentation_up;
				log ("building package variant ...", log_threshold + 1);
				type_libraries.update_element ( 
					container	=> component_libraries,
					position	=> lib_cursor,
					process		=> locate_component'access);
				
				log_indentation_down;
			end build_package_variant;
			

			
		begin -- read_library
			log_indentation_up;
			
			log ("components", log_threshold + 1);
			log_indentation_up;
			
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				-- The schematic library files use comments (#). But only the comments at the begin
				-- of a line are relevant. Others are to be ignored. Thus test_whole_line is false.
				line := read_line(
							line => get_line,
							comment_mark => "#",
							test_whole_line => false,
							number => ada.text_io.line(current_input));
				
				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- Wait for component header like "DEF 74LS00 U 0 30 Y Y 4 F N".
						-- Once the header was read, the component_entered flag goes true so that
						-- no further header is expected. Once the component footer (ENDDEF) has been read,
						-- the component_entered flag goes false.
						-- The lines right after the component header are the so called "fields". The fields can be
						-- regarded as attributes (similar to EAGLE). The first four attributes are hard coded in kicad
						-- and are thus always there. In the component library the fields start with "F0" .. "Fn".

						log (text => to_string (line), level => log_threshold + 4);
						
						if not component_entered then 
							
							if field (line,1) = et_kicad.def then
								component_entered := true;

								init_temp_variables;
								
								-- Since we are reading the fields, we set the active_section to "fields"
								active_section := fields;

								-- The commponent header provides the first component properties:
								tmp_component_name := et_libraries.type_component_generic_name.to_bounded_string (field (line,2)); -- 74LS00

								-- The generic component name must be checked for invalid characters.
								-- NOTE: we test against the kicad specific character set that allows a tilde.
								check_generic_name_characters (
									name => tmp_component_name,
									characters => component_generic_name_characters);
								
								-- for the log:
								--log (field (line,2), log_threshold + 1); -- 74LS00
								log (to_string (tmp_component_name), log_threshold + 1); -- 74LS00

								-- From the header we extract some basic information about the component:
								
								-- The line it is about looks like:  DEF 74LS00 U 0 30 Y Y 4 F N
								-- The fields meaning is as follows:
								--  #2 : name, like 74LS00
								--  #3 : prefix, like U
								--  #4 : unknown, always zero. CS What is it good for ?
								--  #5 : pin name position offset of supply pins, if "place pin names inside" is off. the offset assumes zero
								--  #6 : show pin/pad number Y/N,
								--  #7 : show pin/port name Y/N,
								--  #8 : units total, -- like 4
								--  #9 : all units not interchangeable L (otherwise F), (similar to swap level in EAGLE)
								--  #10: power symbol P (otherwise N)

								tmp_prefix := type_component_prefix.to_bounded_string (field (line,3)); -- U

								-- Detect invalid characters in tmp_prefix:
								-- NOTE: we test against the kicad specific character set that allows a #
								check_prefix_characters (
									prefix => tmp_prefix,
									characters => et_kicad.component_prefix_characters);

								-- The unknown field #4 is always a zero
								if field (line, 4) /= "0" then
									log (message_warning & "expect 0 in field #4 !");
								end if;
								
								tmp_port_name_offset	:= mil_to_distance (mil => field  (line,5), warn_on_negative => false); -- relevant for supply pins only
								tmp_terminal_name_visible	:= to_pin_visibile (field (line,6));
								tmp_port_name_visible	:= to_port_visibile (field (line,7));
								
								-- Get number of units and set swap level as specified in field #9.
								-- Swap level assumes default if only one unit available.
								tmp_units_total := type_units_total'value (field  (line,8));
								if tmp_units_total > 1 then
									log_indentation_up;
									log ("with" & type_units_total'image (tmp_units_total) & " units", log_threshold + 2);

									-- From the "interchangeable" flag we set the component wide swap level. It applies for 
									-- all units of the component (except extra units):
									tmp_unit_swap_level := to_swap_level (field (line,9));
									log_indentation_down;
								else
									tmp_unit_swap_level := unit_swap_level_default;
								end if;

								-- read the appearance flag (N/P) in subfield #10
								-- This is about a component in a library -> schematic => false
								tmp_appearance := to_appearance (line => line, schematic => false);

							end if;
						else -- we are inside a component section and process subsections

							-- We wait for the end of component mark (ENDDEF) and clear the component_entered flag accordingly.
							if field (line,1) = et_kicad.enddef then
								component_entered := false;

								-- Set placeholders (reference, value, commissioned, ...) in internal units.
								-- The placeholder properties are known from the field-section.
								-- The placeholder properties apply for all units.
								set_text_placeholder_properties (component_libraries);

								-- If this is a real component, build package variant from tmp_terminal_port_map
								if tmp_appearance = sch_pcb then
									build_package_variant;
								end if;
								
								-- log component properties
								-- CS: currently no need to output a summary of the component
								--if log_level >= log_threshold + 1 then
								--	et_libraries.write_component_properties (component => comp_cursor);
								--end if;
							else
							-- As long as the component end mark does not appear, we process subsections as 
							-- indicated by active_section:
								log_indentation_up;
							
								case active_section is
									when fields =>
										-- Here we read the "fields". 
										-- There is no end mark for the field list. 
										-- NOTE #1: The only way to detect the end of the field list is to wait for the
										-- header of the footprint list ($FPLIST) or the header of the "draw" list (DRAW).
										-- Then the active_section is set accordingly.

										-- We wait for the header of the footprint or draw list like "$FPLIST" or "DRAW"
										-- and set active_section accordingly.
										-- The component wide data is complete at this time. The component
										-- is to be inserted into the library without any units. Units are assembled and 
										-- added to the component when the section "DRAW" is processed..
										
										-- As long as none of those headers occurs, we read the text fields.
										if field (line,1) = et_kicad.fplist then
											
											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
												container	=> component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;
											
											active_section := footprints;
											--log ("footprint/package filter begin", level => log_threshold + 1);
											log ("footprint/package filter", level => log_threshold + 2);

										elsif field (line,1) = et_kicad.draw then

											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
												container	=> component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;

											active_section := draw;
											log ("draw begin", level => log_threshold + 3);
										else
											-- Read the text fields in a set of temporarily variables field_prefix, field_value, ...
											read_field (line, log_threshold + 2);
										end if;

									when footprints =>
										-- Here we read the footprint list (similar to package variants in EAGLE):

										-- The list of footprints looks like this :

										-- $FPLIST
										--  Pin_Header_Straight_1X06
										--  Pin_Header_Angled_1X06
										--  Socket_Strip_Straight_1X06
										--  Socket_Strip_Angled_1X06
										-- $ENDFPLIST

										-- As long as the footer of the list ($ENDFPLIST) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- that this subsection has been processed.
										if field  (line,1) = et_kicad.endfplist then
											active_section := none;
											--log ("footprint/package filter end", level => log_threshold + 1);
										else
											-- Process lines:
											add_footprint (line, log_threshold + 2);
										end if;

									when draw =>
										-- Here we read the drawing list where lines, arcs and pins are.
										
										-- As long as the footer of the list (ENDDRAW) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- thate this subsection has been processed.
										if field  (line,1) = et_kicad.enddraw then
											active_section := none;
											log ("draw end", level => log_threshold + 3);
										else
											-- Read draw objects
											read_draw_object (line, log_threshold + 2);
										end if;
										
									when none =>
										-- If no subsection is being processed, we wait for the "draw" header (DRAW)
										-- and set the active_section accordingly.
										-- NOTE #2: the active section "fields" is not set here but when the fields are read (see NOTE #1)
										if field  (line,1) = et_kicad.draw then
											active_section := draw;
											log ("draw begin", level => log_threshold + 3);
										end if;

								end case; -- active_section

								log_indentation_down;
								
							end if;
							
						end if; -- inside component section
				end case;

			end loop;

			log_indentation_down;
			log_indentation_down;

			exception
				when constraint_error =>
					log (text => message_error & affected_line (line) & to_string (line), console => true);
					raise;
		end read_library;

		use et_schematic.type_rig;
		
	begin -- read_components_libraries
		et_schematic.reset_library_cursor (project_lib_cursor);
		-- CS: use query_element where module_cursor points to, query libraries ...
		-- CS: remove et_schematic.reset_library_cursor and et_schematic.number_of_libraries
		
		-- If there are no libraries in the project file, there is nothing to do but writing a warning:
		if et_schematic.number_of_libraries = 0 then -- CS: use length
			log (message_warning & "no component libraries defined in project file !");
		else
			log ("Loading component libraries ...", log_threshold);
			log_indentation_up;
			
			-- We loop in the list of project libraries:
			while project_lib_cursor /= type_full_library_names.no_element loop

				-- Set the library to be read:
				lib_file_name := element (project_lib_cursor);

				-- log library file name
				log (to_string (lib_file_name), log_threshold + 1);
				
				if exists (to_string (lib_file_name)) then
					open (
						file => library_handle,
						mode => in_file,
						name => to_string (lib_file_name));

					-- Since the full libary file name (incl. path) is known, we insert an empty
					-- library in the list of component libraries.
					-- After that the lib_cursor points to the latest inserted library.
					type_libraries.insert (
						container	=> component_libraries,
						key			=> lib_file_name, -- full library file name (incl. path) like "../lib/my_lib.lib"
						new_item	=> type_components.empty_map,
						position	=> lib_cursor,
						inserted	=> lib_inserted
						);

					-- The library could have been inserted earlier by reading another project,
					-- thus the library is skipped.
					if lib_inserted then
						-- Now we read the library file and add components
						-- to the library pointed to by lib_cursor:
						set_input (library_handle);
						read_library (log_threshold + 1);
					else
						log (" already loaded -> skipped", log_threshold + 1);
					end if;
					
					close (library_handle);
				else
					log (message_warning & "library '" & to_string (lib_file_name) & "' not found !");
				end if;

				-- prepare next library file to be be read
				next (project_lib_cursor);

			end loop;
			
			log_indentation_down;
			
		end if;
				
	end read_components_libraries;


	function to_package_variant (
	-- Used when reading schematic. Returns the package variant of a component.
	-- Input parameters: the full name of the component library, generic name therein,
	-- name of package library and package name.
		component_library 	: in et_libraries.type_full_library_name.bounded_string; 		-- ../lbr/bel_logic.lib
		generic_name 		: in et_libraries.type_component_generic_name.bounded_string; 	-- 7400
		package_library 	: in et_libraries.type_library_name.bounded_string; 			-- bel_ic
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		log_threshold		: in et_string_processing.type_log_level)
		return et_libraries.type_component_variant_name.bounded_string is 					-- D

		use et_libraries;
		library_cursor : type_libraries.cursor; -- points to the component library
		
		use et_string_processing;
		variant : type_component_variant_name.bounded_string; -- variant name to be returned
		
		-- temporarily here the name of the package library is stored:
		use type_full_library_name;
		full_package_library_name : type_full_library_name.bounded_string; -- ../lbr/bel_ic
		
		procedure locate_component (
		-- Locates the given generic component in the component libraray.
			library_name	: in type_full_library_name.bounded_string;
			components 		: in type_components.map) is

			use type_components;
			component_cursor : type_components.cursor; -- points to the generic component
			
			procedure query_variants (
			-- Queries the package variants of the generic component.
				component_name	: in type_component_generic_name.bounded_string; -- RESISTOR
				component 		: in type_component) is

				use type_component_package_name;
				use type_component_variants;
				use type_component_variant_name;

				-- This cursor points to the package variant being queryied.
				variant_cursor : type_component_variants.cursor := component.variants.first;

			begin -- query_variants
				log ("querying package variants ...", log_threshold + 2);
				log_indentation_up;

				-- Loop through package variants:
				while variant_cursor /= type_component_variants.no_element loop

					-- From the library and package name we can reason the variant name.
					-- So if both the given library and package name match, the variant name
					-- is set to be returned.
					if 	element (variant_cursor).packge.library = full_package_library_name and
						element (variant_cursor).packge.name = package_name then 
						
						log ("variant " 
							& to_string (package_variant => key (variant_cursor)) 
							& " used", log_threshold + 1);

						variant := key (variant_cursor);
						exit; -- no further search required
						
					else
						-- Package variant not defined in library. Make sure
						-- the terminal_port_map (there is only one) can be applied 
						-- on this package variant.
						log (message_error & "unknown variant used !", console => true);
						
						if et_pcb.terminal_port_map_fits (
							library_name 		=> full_package_library_name,
							package_name 		=> package_name,
							terminal_port_map	=> element (variant_cursor).terminal_port_map) then

							-- CS: update library with new variant
							null;
						else
							log_indentation_reset;
							log ("terminal-port map does not fit !", console => true); -- CS: more details
							raise constraint_error; -- CS
						end if;
					end if;

					next (variant_cursor);
				end loop;

				log_indentation_down;
				
				exception
					when event:
						others =>
							log_indentation_reset;
							put_line (ada.exceptions.exception_message (event));
							raise;

			end query_variants;
			
		begin -- locate_component
			log ("locating generic component in library ...", log_threshold + 1);
			log_indentation_up;

			-- Locate the component in the library by its generic name.
			-- If not found, search the component again with a tilde prepended to
			-- to the generic name:
			component_cursor := components.find (generic_name);
			if component_cursor = type_components.no_element then
				component_cursor := components.find (prepend_tilde (generic_name));
			end if;

			-- query the package variants of the generic component
			type_components.query_element (
				position 	=> component_cursor,
				process 	=> query_variants'access);

			log_indentation_down;

			exception
				when event:
					others =>
						log_indentation_reset;
						put_line (ada.exceptions.exception_message (event));
						raise;

		end locate_component;
		
	begin -- to_package_variant
		log ("making package variant ...", log_threshold);
		log_indentation_up;

		-- Compose the full name of the package library:
		full_package_library_name := to_full_library_name (root_dir => lib_dir, lib_name => package_library);

		-- locate the given component library
		library_cursor := component_libraries.find (component_library);

		-- locate the given generic component
		type_libraries.query_element (
			position	=> library_cursor,
			process		=> locate_component'access);
		
		log_indentation_down;

		return variant;
	end to_package_variant;

	
	procedure import_design (
		first_instance 	: in boolean := false;
		project			: in et_schematic.type_project_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Imports the design as specified by project.
	-- Inserts the created submodule in the rig (see et_schematic.type_rig).
	-- Leaves the global module_cursor pointing where the module was inserted.
	-- If first_instance is false, the module gets the name as defined in the kicad project file.
	-- For a regular single design import this is the default.
	-- If first_instance is true, the module name further-on gets the instance appended.
	-- This is required for multiple design instantiations. (things like nucleo_core_1).
		
		--use et_import.type_schematic_file_name;
		use et_libraries.type_library_directory;
		use et_schematic;

		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line;
		
		list_of_submodules : type_submodule_names_extended;

		current_schematic	: type_schematic_file_name.bounded_string;
		
		net_id : natural := 0; -- for counting name-less nets (like N$1, N$2, N$3, ...)

		package stack_of_sheet_lists is new et_general.stack_lifo (max => 10, item => type_submodule_names_extended);
        use stack_of_sheet_lists;
		
		function read_project_file return type_schematic_file_name.bounded_string is
		-- Reads the project file in terms of LibDir and LibName. 
		-- LibDir is stored in variable lib_dir.
		-- Project library names are stored in project_libraries.
		-- Returns the name of the top level schematic file.
			line : type_fields_of_line;
			
			use et_libraries;
			use et_schematic;

			-- "section entered flags"
			section_eeschema_entered 			: boolean := false;
			section_eeschema_libraries_entered	: boolean := false;            
		
			procedure clear_section_entered_flags is
			-- clears section_eeschema_entered and section_eeschema_libraries_entered.
			begin
				section_eeschema_entered := false;
				section_eeschema_libraries_entered := false;
			end clear_section_entered_flags;
		
		begin -- read_project_file
			log_indentation_reset;
			log (
				text => "reading project file " 
				 & compose (
					name		=> type_project_name.to_string (project), 
					extension	=> file_extension_project) & " ...",
				level => log_threshold + 1);
			log_indentation_up;

			-- Clear list of project libraries from earlier projects that have been imported.
			-- If we import only one project, this statement does not matter.
			-- In tmp_project_libraries the project libraries are collected. 
			-- Later they become part of the module being processed.
			type_full_library_names.clear (tmp_project_libraries);

			-- Open project file. 
			-- The file name is composed of project name and extension.
			open (
				file => project_file_handle,
				mode => in_file,
				name => compose (
							name		=> et_schematic.type_project_name.to_string (project), 
							extension	=> file_extension_project)
				);
			set_input (project_file_handle);
			
			while not end_of_file loop

				-- Save a line in variable "line" (see et_string_processing.ads)
				line := read_line(
							line => get_line,
							comment_mark => "#", -- use constant comment_mark
							number => ada.text_io.line (current_input),
							ifs => latin_1.equals_sign); -- fields are separated by equals sign (=)

				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when 1 => -- we have a line with just one field. those lines contain headers like "[eeschema]"

						-- test header [eeschema]
						if field (line,1) = project_header_eeschema then
							clear_section_entered_flags;
							section_eeschema_entered := true;
						end if;

						-- test header [eeschema/libraries]
						if field (line,1) = project_header_eeschema_libraries then
							clear_section_entered_flags;
							section_eeschema_libraries_entered := true;
						end if;

					when 2 =>
						if section_eeschema_entered then

							-- Get root path to libraries (LibDir) and store it in lib_dir (see et_libraries.ads)
							-- CS: currently we assume only one path here. Provide procedure that sets lib_dir and checks
							-- deviations from this rule.
							if field (line,1) = project_keyword_library_directory then
								lib_dir := to_bounded_string (field (line,2));

								-- For the log write something like "LibDir ../../lbr"
								log (project_keyword_library_directory 
									 & " " & et_libraries.to_string (lib_dir),
									log_threshold + 2);
							end if;
							
						end if;

						if section_eeschema_libraries_entered then

							-- From a line like "LibName1=bel_supply" get library names (incl. path and extension) and
							-- store them in list tmp_project_libraries (see et_kicad.ads).
							-- We ignore the index of LibName. Since we store the lib names in a 
							-- doubly linked list their order remains unchanged anyway.
							if field (line,1)(1..project_keyword_library_name'length) 
								= project_keyword_library_name then

								type_full_library_names.append (
									container	=> tmp_project_libraries, 
									new_item	=> type_full_library_name.to_bounded_string (
										compose (
											containing_directory	=> et_libraries.to_string (lib_dir),
											name					=> field (line,2),
											extension				=> file_extension_schematic_lib)));
								
								-- For the log write something like "LibName bel_connectors_and_jumpers"
								log (field (line,1) & " " & field (line,2), log_threshold + 2);
							end if;

						end if;
						
					when others => null;
				end case;

				
-- 				if section_eeschema_entered or section_eeschema_libraries_entered then
-- 					put_line(" " & et_string_processing.to_string(line));
-- 				end if;
				
			end loop;

			close (project_file_handle);

			-- Derive the top level schematic file name from the project name.
			-- It is just a matter of file extension.
			return type_schematic_file_name.to_bounded_string (
				compose (
					name		=> et_schematic.type_project_name.to_string (project), 
					extension	=> file_extension_schematic)
					);
		end read_project_file;

		function to_angle (text_in : in string) return et_coordinates.type_angle is
		-- Converts the label orientation to type_angle.
		-- CS: use a dedicated type for input parameter.
			o_in : type_label_orientation := type_label_orientation'value(text_in);
			o_out : et_coordinates.type_angle;
		begin
			case o_in is
				when 0 => o_out := 180.0; -- CS: probably 0.0 ?
				when 1 => o_out :=  90.0;
				when 2 => o_out :=   0.0; -- CS: probably 180.0 ?
				when 3 => o_out := 270.0;
			end case;
			return o_out;
			-- CS: exception handler
		end to_angle;
		
		function to_direction (text_in : in string) return type_label_direction is
		-- Converts the direction of a label to a type_label_direction. 
		-- CS: currently case sensitive ! Use dedicated type for input parameter.
			d_out : type_label_direction := input;
		begin
			if text_in = schematic_keyword_label_dir_input then
				d_out := input;
			elsif text_in = schematic_keyword_label_dir_output then
				d_out := output;
			elsif text_in = schematic_keyword_label_dir_bidir then
				d_out := bidir;
			elsif text_in = schematic_keyword_label_dir_tristate then
				d_out := tristate;
			elsif text_in = schematic_keyword_label_dir_passive then
				d_out := passive;
			else
				log_indentation_reset;
				log (message_error & "Label direction unknown !", console => true);
				raise constraint_error;
			end if;
			
			return d_out;
		end to_direction;		

		-- Prodcedures that set the s,e or picked flag in a wild net segment. 
		-- CS: move them to function search_for_same_coordinates
		procedure set_e (segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
		procedure set_s (segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;

		-- CS: move to procedure add_segment_to_anonymous_strand
		procedure set_picked (segment : in out type_wild_net_segment ) is begin segment.picked := true; end set_picked;
		
		function read_schematic (
			current_schematic	: in type_schematic_file_name.bounded_string;
			log_threshold		: in type_log_level)
			return type_submodule_names_extended is
		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
        -- they will be returned in list_of_submodules. Otherwise the returned list is empty.

			list_of_submodules : type_submodule_names_extended; -- list to be returned
			name_of_submodule_scratch : type_submodule_name.bounded_string; -- temporarily used before appended to list_of_submodules

			use et_string_processing;
		
			line : et_string_processing.type_fields_of_line; -- the line of the schematic file being processed
		
			sheet_file : type_schematic_file_name.bounded_string;
			sheet_count_total, sheet_number_current : type_submodule_sheet_number;

			wild_simple_labels	: type_simple_labels.list;
            wild_tag_labels 	: type_tag_labels.list;
			wild_segments		: type_wild_segments.list;
			wild_junctions		: type_junctions.list;

			-- In the first stage, all net segments of this sheet go into a wild collection of segments.
			-- Later they will be sorted and connected by their coordinates (start and and points)
			segment_count	: count_type := 0; -- holds the total number of segments within a sheet
			
			anonymous_strand : type_anonymous_strand;

			-- The list of anonymous strands. Procedure add_strand_to_anonymous_strands uses 
			-- this container for temporarily storage of anonymous strands.
			anonymous_strands : type_anonymous_strands.list; 

			procedure error_in_schematic_file (line : in type_fields_of_line) is
			begin
				log_indentation_reset;
				log (message_error & "in schematic file '" 
					& to_string (current_schematic) & "' " 
					& et_string_processing.affected_line (line)
					& to_string (line),
					console => true);
			end error_in_schematic_file;
		
			procedure add_segment_to_anonymous_strand (segment_cursor : in type_wild_segments.cursor) is
			-- Adds a net segment (indicated by given cursor) to anonymous_strand.
			-- This procedure happens to be called for a certain segment more than once (unavoidable). So the flag "picked" serves
			-- as indicator for a segment already added to the anonymous_strand.
				scratch : type_net_segment;
			begin
				-- If segment already picked and added to anonymous_strand, do nothing with this segment. 
				-- Otherwise set the "picked" flag of that segment, output the coordinates of the segment, add it to anonymous net.
				if type_wild_segments.element (segment_cursor).picked then
					null;
					-- log ("  picked");
				else
					-- log ("  segment" & positive'image(id) & ":");
					-- log ("segment" & positive'image(id) & ":");
					-- log ("  segment" & positive'image(id) & ":");
					
					type_wild_segments.update_element (
						container => wild_segments,
						position => segment_cursor,
						process => set_picked'access);

-- 					write_coordinates_of_segment (segment => 
-- 						type_net_segment (type_wild_segments.element (segment_cursor)));
					
					log (to_string (
							segment => type_net_segment (type_wild_segments.element (segment_cursor)),
							scope => xy), log_threshold + 1);

					scratch := type_net_segment (type_wild_segments.element (segment_cursor));
					type_net_segments.append (anonymous_strand.segments, scratch);
				end if;
			end add_segment_to_anonymous_strand;

			function search_for_same_coordinates (
			-- Starting from a segment indicated by id and the end point (given by side), 
			-- search in wild_segments for a segment with matching start or end point.
			-- In general untouched segments are preferred in the search. "Half" processed segments are of secondary relevance.
			-- Once a suitable segment was found, sc is assigned with neccessary data to be returned to the parent unit. The search for 
			-- a suitable segment excludes fully processed segments and the given segment (id).
				segment_cursor : in type_wild_segments.cursor;
				seg_in : in type_wild_net_segment;
				side : in type_segment_side) 
				return type_same_coord_result is

				sc : type_same_coord_result;
				line_start, line_end : et_coordinates.type_coordinates;
				s, e	: boolean; -- indicate the end point, that has been processed already
				untouched, half_processed : boolean; -- indicate whether a segment is completely untouched or processed in only one direction

				use et_coordinates;
				--use et_libraries;
				use type_wild_segments;
			
				cursor : type_wild_segments.cursor;
			begin -- search_for_same_coordinates
				-- Set E/S flag:
				-- If we start the search from the end_point of a segment, the e-flag is to be set. This indicates the end_point has been processed.
				-- If we start the search from the start_point of a segment, the s-flag is to be set. This indicates the start_point has been processed.				
				case side is
					when end_point =>
						
-- 						log ("--> origin of search   (END): " 
-- 							 & type_grid'image(seg_in.coordinates_end.x) & "/" & type_grid'image(seg_in.coordinates_end.y),
-- 							 level => 1);
						
						type_wild_segments.update_element(
								container => wild_segments,
								position => segment_cursor,
								process => set_e'access);
						
					when start_point =>
						
-- 						log ("--> origin of search (START): " 
-- 							 & type_grid'image(seg_in.coordinates_start.x) & "/" & type_grid'image(seg_in.coordinates_start.y),
-- 							 level => 1);
						
						type_wild_segments.update_element(
								container => wild_segments,
								position => segment_cursor,
								process => set_s'access);
				end case;

				-- First, search completely untouched segments (they have both e and s flag cleared).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				cursor := wild_segments.first;
				while cursor /= no_element loop
					if cursor /= segment_cursor then -- skip the given segment
						line_start := type_wild_segments.element (cursor).coordinates_start;
						line_end   := type_wild_segments.element (cursor).coordinates_end;
						s  := type_wild_segments.element (cursor).s;
						e  := type_wild_segments.element (cursor).e;
						untouched := not (s or e); -- neither s nor e set

						if untouched then 
							--put(et_import.report_handle,"probe untouched segment: ");
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
									if distance_x (line_start) = distance_x (seg_in.coordinates_end) and distance_y (line_start) = distance_y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
									if distance_x (line_end) = distance_x (seg_in.coordinates_end) and distance_y (line_end) = distance_y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.									
								when start_point =>
									--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
									if distance_x (line_start) = distance_x (seg_in.coordinates_start) and distance_y (line_start) = distance_y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
									if distance_x (line_end) = distance_x (seg_in.coordinates_start) and distance_y (line_end) = distance_y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;

					next (cursor);
				end loop;

				-- No untouched segment found.
				-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				cursor := wild_segments.first;
				while cursor /= no_element loop
					if cursor /= segment_cursor then -- skip the given segment
						line_start := type_wild_segments.element (cursor).coordinates_start;
						line_end   := type_wild_segments.element (cursor).coordinates_end;
						s  := type_wild_segments.element (cursor).s;
						e  := type_wild_segments.element (cursor).e;
						half_processed := s xor e;

						if half_processed then
							--put(et_import.report_handle,"probe half-processed segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_segments.element(wild_segments,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
									if distance_x (line_start) = distance_x (seg_in.coordinates_end) and distance_y (line_start) = distance_y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
									if distance_x (line_end) = distance_x (seg_in.coordinates_end) and distance_y (line_end) = distance_y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
								when start_point =>
									--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
									if distance_x (line_start) = distance_x (seg_in.coordinates_start) and distance_y (line_start) = distance_y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
									if distance_x (line_end) = distance_x (seg_in.coordinates_start) and distance_y (line_end) = distance_y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;

					next (cursor);
				end loop;
				
				sc.valid := false;
				sc.cursor := cursor;
				return sc;
				
			<<matching_segment_coordinates_found>>
				add_segment_to_anonymous_strand (sc.cursor);
				--log ("match", level => 1);
				
				return sc;
			end search_for_same_coordinates;
			
			
			procedure associate_net_labels_with_anonymous_strands is
			-- All anonymous strands must be given a name. The name is enforced by net labels.
				
			-- The first label found on the strand dictates the strand name.
			-- Other labels on the strand are checked for their name only. 
			-- If the name differs from the strand name set earlier, an error is output.
			-- If scope of strands are contradicting, error is output.

			-- The kind of net label (simple, hierarchic, global) defines the scope of the strand.
			-- Net labels sitting on a segment, are added to the list of labels of that segment.
			
			-- Strands without label are named by using the notation "N$". 

				use et_coordinates;
			
				ls  :	type_net_label_simple;
				lt  : 	type_net_label_tag;				
				anon_strand_a, anon_strand_b : type_anonymous_strand;
				segment	: type_net_segment;
				lls : 	type_simple_labels.list;
				llt : 	type_tag_labels.list;
			
				strand 		: type_strand;
				net_name	: type_net_name.bounded_string;
				
				function label_sits_on_segment (label : in type_net_label; segment : in type_net_segment) return boolean is
					sits_on_segment : boolean := false;
					d : et_geometry.type_distance_point_from_line;
					use et_geometry;
				begin
					-- calculate the shortes distance of point from line.
					d := distance_of_point_from_line (
						--point		=> et_libraries.type_coordinates(point),
						point		=> type_2d_point (label.coordinates),
						line_start	=> type_2d_point (segment.coordinates_start),
						line_end	=> type_2d_point (segment.coordinates_end),
						line_range	=> with_end_points);
					
					--log ("distance: " & type_grid'image(d.distance), level => 1);
					
					if not d.out_of_range and d.distance = zero_distance then
						sits_on_segment := true;
					end if;
					return sits_on_segment;
				end label_sits_on_segment;

				use type_net_segments;
				-- the segment cursor points to the segment being processed
				segment_cursor : type_net_segments.cursor; 

				use type_anonymous_strands;
				
				-- the strand cursor points to the anonymous strand being processed
				strand_cursor	: type_anonymous_strands.cursor := anonymous_strands.first;
				strand_cursor_b	: type_anonymous_strands.cursor;

				use type_simple_labels;
				simple_label_cursor	: type_simple_labels.cursor; -- points to the simple label being processed

				use type_tag_labels;
				tag_label_cursor	: type_tag_labels.cursor; -- points to the tag label being processed

				use type_net_name;

				procedure output_net_label_conflict is
				begin
					put_line (standard_output, message_error & "Net label conflict !");
				end output_net_label_conflict;
				
			begin -- associate_net_labels_with_anonymous_strands
				log_indentation_up;
				
				-- This does only make sense if there are strands at all:
				if not is_empty (anonymous_strands) then
					log (text => "associating net labels with strands ...");
					
					-- Loop in list of anonymous strands, get a (non-processed-yet) strand, loop in list of segments and 
					-- find a (non-processed-yet) net label that sits on the net segment. If label sits on segment:
					--  - assume label text as name of strand (and check other labels of the anonymous strand)
					--  - set scope of strand according to the net label
					--
					--  - mark label as processed
					--  - update/replace label in wild_simple_labels or wild_tag_labels
					--
					--  - Mark anonymous strand as processed. This indicates that the strand has got a name (given by a label).
					--    Non-Processed strands are those without a label.
					--  - update/replace anonymous strand in anonymous_strands
					while strand_cursor /= type_anonymous_strands.no_element loop -- cursor already reset on declaration (see above)
						anon_strand_a := element (strand_cursor); -- get anonymous strand
						
						--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": "); -- CS: log ?
						if not anon_strand_a.processed then -- skip already processed nets

							-- reset segment cursor to begin of segment list of the anonymous net
							segment_cursor := anon_strand_a.segments.first;
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment in anonymous strand anon_strand_a
								segment := anon_strand_a.segments (segment_cursor);
								--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s); -- CS: log ?
								
								-- Loop in list of simple labels:
								if not is_empty (wild_simple_labels) then -- do it if there are simple labels at all
									--put_line(" simple labels ..."); -- CS: log ?
									simple_label_cursor := wild_simple_labels.first; -- reset label cursor
									while simple_label_cursor /= type_simple_labels.no_element loop
										ls := element (simple_label_cursor); -- get simple label
										
										if not ls.processed then
											--put(et_import.report_handle, "   probing "); write_coordinates_of_label( type_net_label(ls));  -- CS: log ?
											if label_sits_on_segment (label => type_net_label(ls), segment => segment) then

												if log_level >= log_threshold + 1 then
													log_indentation_up;
													log ("label at " & to_string (label => type_net_label (ls), scope => xy));
													log_indentation_down;
												end if;

												-- Check if the label does not contradict with other labels of this strand.
												-- Otherwise, set scope to local.
												case anon_strand_a.scope is
													when unknown => -- find. no label found so far. set scope of strand
														anon_strand_a.scope := local;

													when local => -- strand has been marked as "local" already. nothing to to
														null;

													when hierarchic => -- strand has been marked as "hierarchic" already. no local label allowed !
														output_net_label_conflict;
														log_indentation_reset;
														log (message_error
															& "hierarchic net " & type_net_name.to_string (anon_strand_a.name) 
															& " has a local label at " 
															& to_string (position => ls.coordinates) & " !");
														raise constraint_error;

													when global => -- strand has been marked as "global" already. no local label allowed !
														output_net_label_conflict;
														log_indentation_reset;
														log (message_error
															& "global net " & type_net_name.to_string (anon_strand_a.name) 
															& " has a local label at " 
																& to_string (position => ls.coordinates) & " !");
															raise constraint_error;
												end case;

												
												-- The first matching simple label dictates the strand name. 
												-- If other labels with text differing from strand name found, output warning.
												if type_net_name.length (anon_strand_a.name) = 0 then -- If this is the first matching label

													-- assume the label text as strand name.
													anon_strand_a.name := ls.text; 
												else
													-- If label text is different from previously assigned strand name:
													if not type_net_name."=" (anon_strand_a.name, ls.text) then
														output_net_label_conflict;

														-- for the log, some more information
														log_indentation_reset;
														log (message_error 
															 & "Net " & type_net_name.to_string (anon_strand_a.name) & " has contradicting label " 
															 & "at " & to_string (position => ls.coordinates) & " !");
														raise constraint_error;
													end if;
												end if;

												-- mark simple label as processed and update/replace it in wild_simple_labels
												ls.processed := true;
												type_simple_labels.replace_element (
													container => wild_simple_labels,
													position => simple_label_cursor,
													new_item => ls);

												-- Collect simple label (ls) in temporarily list of simple labels (lls).
												type_simple_labels.append (lls,ls);

												-- Mark anonymous strand as processed.
												anon_strand_a.processed := true;
											end if;
										end if;

										next (simple_label_cursor); -- advance label cursor
									end loop;

									-- Copy list of simple labels (lls) to current segment (s).
									segment.label_list_simple := lls;
									
									-- Update/replace segment in current anonymous strand.
									type_net_segments.replace_element (
										container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
										position => segment_cursor,
										new_item => segment); -- the updated segment
									
									-- Clean up: Purge temporarily list of simple labels for next spin.
									type_simple_labels.clear (lls);

									-- Update/replace anonymous net in anonymous_nets.
									type_anonymous_strands.replace_element (
										container => anonymous_strands, -- the list of anonymous strands
										position => strand_cursor,
										new_item => anon_strand_a); -- the updated anonymous net
								end if;
								
								-- Loop in list of tag labels:
								if not is_empty (wild_tag_labels) then -- do if if there are tag labels at all
									--put_line(" hierarchic and global labels ...");	 -- CS: log ?

									tag_label_cursor := wild_tag_labels.first; -- reset label cursor
									while tag_label_cursor /= type_tag_labels.no_element loop
										lt := element (tag_label_cursor); -- get tag label
										
										if not lt.processed then								
											if label_sits_on_segment (label => type_net_label (lt), segment => segment) then

												if log_level >= log_threshold + 1 then
													log_indentation_up;
													log ("label at " & to_string (label => type_net_label (lt), scope => xy));
													log_indentation_down;
												end if;

												-- Check if the label does not contradict with other labels of this strand.
												-- Otherwise, set scope according to the label just found.
												case anon_strand_a.scope is
													when unknown => -- find. no label found so far. set scope of strand
														if lt.global then 
															anon_strand_a.scope := global;
														end if;
														if lt.hierarchic then 
															anon_strand_a.scope := hierarchic;
														end if;

													when local => -- strand has been marked as "local" already. no hierarchic or global label allowed !
														if lt.global or lt.hierarchic then
															output_net_label_conflict;
															log_indentation_reset;
															log (message_error
																& "local net " & type_net_name.to_string (anon_strand_a.name) 
																& " has a hierarchic or global label at " 
																& to_string (position => lt.coordinates) & " !");
															raise constraint_error;
														end if;
														
													when hierarchic => -- strand has been marked as "hierarchic" already. no global label allowed !
														if lt.global then
															output_net_label_conflict;
															log_indentation_reset;
															log (message_error
																& "hierarchic net " & type_net_name.to_string (anon_strand_a.name) 
																& " has a global label at " 
																& to_string (position => lt.coordinates) & " !");
															raise constraint_error;
														end if;

													when global => -- strand has been marked as "global" already. no hierarchic label allowed !
														if lt.hierarchic then
															output_net_label_conflict;
															log_indentation_reset;
															log (message_error
																& "global net " & type_net_name.to_string (anon_strand_a.name) 
																& " has a hierarchic label at " 
																& to_string (position => lt.coordinates) & " !");
															raise constraint_error;
														end if;
												end case;

												-- The first matching label dictates the net name and scope. 
												-- If other labels with text differing from net name found, output warning.
												if type_net_name.length (anon_strand_a.name) = 0 then -- If this is the first matching label
													anon_strand_a.name := lt.text; -- assume the label text as net name.
												else
													-- If label text is different from previously assigned net name:
													if anon_strand_a.name /= lt.text then 
														log_indentation_reset;
														log (message_error 
															 & "Net " & type_net_name.to_string (anon_strand_a.name) & " has contradicting label " 
															 & "at " & to_string (position => lt.coordinates) & " !");
														raise constraint_error;
													end if;
												end if;

												-- mark tag label as processed and update/replace it in wild_tag_labels
												lt.processed := true;
												type_tag_labels.replace_element (
													container => wild_tag_labels,
													position => tag_label_cursor,
													new_item => lt);

												-- Collect tag label (lt) in temporarily list of simple labels (llt).
												type_tag_labels.append (llt,lt);

												-- Mark anonymous net as processed.												
												anon_strand_a.processed := true;
											end if;
										end if;

										next (tag_label_cursor);
									end loop;

									-- Copy list of tag labels (llt) to current segment (s).
									segment.label_list_tag := llt;
									-- Update/replace segment in current anonymous net.
									type_net_segments.replace_element (
										container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
										position => segment_cursor,
										new_item => segment); -- the updated segment

									-- Clean up: Purge temporarily list of tag labels for next spin.
									type_tag_labels.clear (llt);

									-- Update/replace anonymous net in anonymous_nets.
									type_anonymous_strands.replace_element (
										container => anonymous_strands, -- the list of anonymous strands
										position => strand_cursor,
										new_item => anon_strand_a); -- the updated anonymous net
								end if;

								next (segment_cursor); -- advance segment cursor
							end loop;
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					-- Build name-less strands from anonymous strands.
					-- Anonymous strands have no label, hence no name -> "processed" flag is still cleared.
					-- As placeholder for the name we use the notation "N$n" where n is taken from the net_id (counter of name-less strands)
					-- Their scope is strictly "local".
					--
					-- We build a new strand (of type type_strand_named) in an intermediate variable "strand"
					-- (as specified in et_schematic type_strand_named) for transfer to the module netlist.
					--
					-- NOTE: Even if a strand has no name at this stage, it may get a dedicated name later.
					-- Power-out ports may overwrite the strand name.
					log (text => "building name-less strands ...");
					log_indentation_up;

					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get anonymous strand

						if not anon_strand_a.processed then

							-- build temporarily strand with a name like N$542
							net_id := net_id + 1; -- increment net id. net_id applies for the whole design. see declarations of procedure import_design
							net_name := type_net_name.to_bounded_string (
								anonymous_net_name_prefix & trim (natural'image (net_id), left));

							log (type_net_name.to_string (net_name), level => 2);
							
							strand.name := net_name;
							strand.scope := local;

							log_indentation_up;
							log ("scope " & to_string (strand.scope) & " with segments", level => 2);
							
							-- fetch net segments from anonymous strand and append them to the new name-less strand:
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous net
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous strand anon_strand_a
								segment := element (segment_cursor); -- get segment
								type_net_segments.append (container => strand.segments, new_item => segment);
								
								if log_level >= 2 then
									--write_coordinates_of_segment (segment => segment);
									log_indentation_up;
									log (to_string (segment => segment, scope => xy));
									log_indentation_down;
								end if;
								
								next (segment_cursor);
							end loop;

							log_indentation_down;
							
                            -- assign coordinates
							set_path (strand.coordinates, path_to_submodule);
							set_sheet (strand.coordinates, sheet_number_current);

							-- set x,y coordinates (lowest available on the sheet)
							set (strand.coordinates, to_coordinates (lowest_xy (strand, log_threshold + 3)));
                            
							-- insert strand in module, then purge strand.segments for next spin
							log ("inserting strand in module ...", log_threshold + 2);
							add_strand (strand);

							type_net_segments.clear (strand.segments);
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;
					
					log_indentation_down;
					
					-- Build named strands with label. Those strands have the "processed" flag set.
					-- NOTE: Even if a strand has a dedicated name at this stage, it may get a dedicated name later on netlist generation.
					-- Power-out ports may overwrite the strand name (which would be regarded as design error and is handled on netlist generation)
					log (text => "building named strands ...");
					log_indentation_up;
					
					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get a strand

						if anon_strand_a.processed then -- it must have a name

							log (type_net_name.to_string (anon_strand_a.name), level => 2);
							
							strand.name := anon_strand_a.name;
							strand.scope := anon_strand_a.scope;

							log_indentation_up;
							log ("scope " & to_string (strand.scope) & " with segments", level => 2);

							-- fetch net segments from anonymous strand and append them to the new named strand:
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous strand
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous_strand "a"
								segment := element (segment_cursor); -- get segment
								type_net_segments.append (container => strand.segments, new_item => segment);
								
								if log_level >= 2 then
									--write_coordinates_of_segment (segment => segment);
									log (to_string (segment => segment, scope => xy));
								end if;
								
								next (segment_cursor);
							end loop;

							log_indentation_down;

                            -- assign coordinates
                            set_path (strand.coordinates, path_to_submodule);
							set_sheet (strand.coordinates, sheet_number_current);

							-- set x,y coordinates (lowest available on the sheet)
							set (strand.coordinates, to_coordinates (lowest_xy (strand, log_threshold + 3)));
							
							-- insert strand in module, then purge strand.segments for next spin
							log ("inserting strand in module ...", log_threshold + 2);
							add_strand (strand);
							type_net_segments.clear (strand.segments);

						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					log_indentation_down;
					
				else
					log (message_note 
						 & "The schematic does not contain nets to associate net labels with !");
				end if;

				log_indentation_down;
			end associate_net_labels_with_anonymous_strands;
			
			procedure process_junctions is
			-- Breaks down all net segments where a junction sits on. 
			-- In the end, the number of net segments may increase.

			-- NOTE: The junction to be tested is taken from the wild list of net junctions. This
			-- list contains the junction of the current sheet exclusively.
			
			-- Loops in wild_segments and tests if a junction sits on a segment.
			-- Then splits the segment where the junction sits. If there are junctions left on 
			-- the remaining fragments, they will be detected in the next spin. 
			-- The flag segment_smashed indicates there are no more segments left with a junction.
				segment : type_wild_net_segment;
				junction : type_net_junction;
			
				use et_schematic.type_junctions;
				junction_cursor : et_schematic.type_junctions.cursor; -- points to the junction being processed

				procedure change_segment_start_coordinates (segment : in out type_wild_net_segment) is 
				begin
					segment.coordinates_start := junction.coordinates;
				end change_segment_start_coordinates;
				
				segment_smashed : boolean := true; -- indicates whether a segment has been broken down

				use type_wild_segments;
				segment_cursor : type_wild_segments.cursor; -- points to the current segment
				
			begin -- process junctions
				
				log_indentation_up;
				
				-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
				-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
				if not is_empty (wild_junctions) then 
					log ("processing" & count_type'image (length (wild_junctions)) & " net junctions ...", log_threshold);

					-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
					-- does not increase anymore, all segments are processed.
					while segment_smashed loop
						
						segment_cursor := wild_segments.first;
						loop_s:
						while segment_cursor /= type_wild_segments.no_element loop
						
							segment := type_wild_segments.element (segment_cursor); -- get a segment

							-- loop in wild junction list until a junction has been found that sits on the segment
							junction_cursor := wild_junctions.first; -- reset junction cursor to begin of junction list
							while junction_cursor /= type_junctions.no_element loop

								-- fetch junction from current cursor position
								junction := type_junctions.element (junction_cursor);
								
								if junction_sits_on_segment (junction, type_net_segment (segment)) then -- match

									if log_level >= log_threshold + 1 then
										log_indentation_up;
										log (to_string (position => junction.coordinates, scope => xy));
										log_indentation_down;
									end if;
									-- NOTE: junctions sitting on a net crossing may appear twice here.

									-- move start coord. of the current segment to the position of the junction
									type_wild_segments.update_element (
										container => wild_segments,
										position => segment_cursor,
										process => change_segment_start_coordinates'access
										);

									-- replace end coord. of segment by pos. of junction
									segment.coordinates_end := junction.coordinates;

									-- append new segment to list of wild segments
									type_wild_segments.append (
										container => wild_segments,
										new_item => segment
										);

									exit loop_s;
								end if;

								next (junction_cursor);
							end loop;

							next (segment_cursor);
						end loop loop_s;

						-- Test if segment_count has increased. If yes, set segment_smashed flag so that the wild_segments
						-- can be searched again. Otherwise clear segment_smashed -> end of procedure.
						if type_wild_segments.length (wild_segments) > segment_count then
							segment_smashed := true;
							-- update segment_count (should increment by 1)
							segment_count := type_wild_segments.length (wild_segments);
						else
							segment_smashed := false;							
						end if;
					end loop;

					log ("update: net segments total" & count_type'image (segment_count), log_threshold);
				end if;

				log_indentation_down;
			end process_junctions;


			procedure build_anonymous_strands is
			-- From the wild segments and junctions assemble net segments to anonymous strands.

				procedure add_strand_to_anonymous_strands is
				-- Once an anonymous strand is complete, it gets appended to a list of anonymous strands. 
				-- Afterward the anonymous strand is deleted. It is a list of net segments which must be purged so that the list
				-- "anonymous_strand" can be filled with net segments of the next anonymous strand.
				begin
					type_anonymous_strands.append (anonymous_strands, anonymous_strand);
					type_net_segments.clear (anonymous_strand.segments);
				end add_strand_to_anonymous_strands;

				use type_wild_segments;

				-- primary and secondary segment cursors.
				segment_cursor_a, segment_cursor_b : type_wild_segments.cursor;

				-- node of the segment (end or start point)
				side : type_segment_side;

				-- the result of a segment search
				search_result : type_same_coord_result;
				
			begin -- build_anonymous_strands
				log_indentation_up;
				
				-- Build anonymous nets:
				-- We are processing the net segments of a sheet here. The net segments have been collected in 
				-- a wild collection of net segments earlier.
				-- This wild collection of segments does not reveal the actual nets where the segments belong to.
				-- The segments are inspected
				-- in the following by looking at the coordinates of their start and end points. 
				-- Segments whose start or end points match other segments are considered
				-- as connected to each other (means they belong to the same strand).
				-- The net name is unknown yet. So the outcome of the following is a list of anonymous strands.
				
				-- CS: handle circlular strands, currently they cause a forever-loop here
				
				segment_count := type_wild_segments.length (wild_segments); -- get number of segments on the current sheet

				log ("processing" & count_type'image (segment_count) & " net segments ...", log_threshold);

				-- It may happen that a sheet has no nets, for example the top level sheet of a design.
				-- If there are no net segments at all, nothing happens.
				if segment_count > 0 then 

					-- Segments where a junction sits on, must be broken down. This results in more segments than calculated earlier.
					-- The outcome of process_junctions might be a greater number of net segments than currently being held in segment_count.
					process_junctions;
					-- segment_count now has been updated

					log_indentation_up;
					
					-- We inspect one segment after another. segment_cursor_a points to the first segment to be processed. 
					-- A segment, whose "e" AND "s" flag has been set, is to be skipped (because this segment has been processed already).
					-- Variable side_scratch points to the side of the segment (start or end point) where another matching segment
					-- is to be searched for.
					-- If a matching segment is found, it gets appended to the current anonymous strand.

					-- set primary segment cursor to begin of wild segment collection
					segment_cursor_a := wild_segments.first; 

					-- The primary segment cursor advances once an anonymous stand is complete (when all connected segments have been found).
					-- Each time a connected segment has been found, the secondary segment cursor points to that segment.
					while segment_cursor_a /= type_wild_segments.no_element loop
						segment_cursor_b := segment_cursor_a;

						-- Already processed segments are skipped. (Processed segments have the "s" and "e" flag set.)
						if not type_wild_segments.element (segment_cursor_b).s and 
						   not type_wild_segments.element (segment_cursor_b).e then 

						    -- We initiate a new strand and start looking for a matching segment on the end_point:
							--put_line(et_import.report_handle," anonymous net" & positive'image(seg) & ":"); 
							log ("assembling strand with segments", log_threshold + 1);
							log_indentation_up;

							-- The first segment is to be added to the anonymous strand.
							add_segment_to_anonymous_strand (segment_cursor_b); 
							side := end_point;

							loop -- A
								--put_line(et_import.report_handle,"  --> A"); -- CS: log ?
								
								-- Search for a segment connected to the current segment. 
								-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment
								-- to current anonymous strand.
								-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to indicate
								-- which end point has been processed.
								-- If no connected segment found, toggle side and repeat search_for_same_coordinates
								-- on the opposide of the segment.
								search_result := search_for_same_coordinates (
									segment_cursor	=> segment_cursor_b,
									seg_in			=> type_wild_segments.element (segment_cursor_b),
									side			=> side);

								if search_result.valid then
									--put_line(et_import.report_handle,"  --> E"); -- CS: log ?
									null;
								else
									-- Toggle side_scratch depending on the e/s flag of the segment:
									-- D
 									if type_wild_segments.element (segment_cursor_b).e then
										-- put_line(et_import.report_handle,"  --> D1"); -- CS: log ?
										side := start_point;
									end if;
									
 									if type_wild_segments.element (segment_cursor_b).s then
										-- put_line(et_import.report_handle,"  --> D2"); -- CS: log ?
 										side := end_point;	
 									end if;

									-- C
									--put_line(et_import.report_handle,"  --> C"); -- CS: log ?
									
									-- Search for a segment connected to the current side of the segment.
									-- If function search_for_same_coordinates discovers a suitable segment, 
									-- it adds the segment to current anonymous strand.
									-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to
									-- indicate which end point has been processed.
									-- If no connected segment found, the current anonymous net is considered
									-- as complete -> cancel loop, advance to next segment ...
									search_result := search_for_same_coordinates (
										segment_cursor	=> segment_cursor_b,
										seg_in			=> type_wild_segments.element (segment_cursor_b),
										side			=> side);
									
									if search_result.valid then
										--put_line(et_import.report_handle,"  --> F"); -- CS: log ?
										null;
									else
										--put_line(et_import.report_handle,"  done"); -- CS: log ?
										
										-- All collected segments belong to the same net.
										-- This net is to be added to the list of anonymous nets.
										add_strand_to_anonymous_strands; 	
																			
										exit;	-- no further segment search required.
									end if;
								end if;

								-- B
								--put_line(et_import.report_handle,"  --> B"); -- CS: log ?
								
								-- Update secondary segment_cursor with the cursor of the segment just found 
								-- by search_for_same_coordinates. So the secondary segment cursor now points to the next
								-- connected segment. 
								-- Same_coord_result contains the end point of the segment that has just been found.
								-- Depending on the end point of the matching segment, side must be set so that the
								-- search can continue on the opposide of the new segment.
								segment_cursor_b := search_result.cursor;
								
								case search_result.side is
									when end_point => 
										side := start_point;
									when start_point =>
										side := end_point;
								end case;
							end loop;

							log_indentation_down;
						end if;

						-- advance primary segment cursor
						next (segment_cursor_a);
					end loop;

					log_indentation_down;

				end if;

				log_indentation_down;

			end build_anonymous_strands;

		
			procedure check_header (line : in type_fields_of_line) is
			-- Tests the given line if it contains a valid schematic sheet header.
			-- Sets the flag schematic_headline_processed.
			-- Aborts program if schematic version invalid.
			begin
				if field (line,1) = schematic_header_keyword_sys_name and
					field (line,2) = schematic_header_keyword_schematic and
					field (line,3) = schematic_header_keyword_file and
					field (line,4) = schematic_header_keyword_version then
						if positive'value (field (line,5)) = schematic_version then
							-- headline ok, version is supported
							schematic_version_valid := true;
						else
							log_indentation_reset;
							log (text => message_error & "schematic version" 
									& positive'image(schematic_version) & " required.",
								console => true);
							raise constraint_error;
						end if;
				end if;
			end check_header;
			
			procedure make_sheet_header (lines : in type_lines.list) is
			-- Builds the sheet header.
			-- The sheet header mainly contains the used libraries.

				sheet_header : type_sheet_header; -- the header being built
			
				--	LIBS:nucleo_core-rescue
				--	LIBS:power
				-- 	LIBS:bel_connectors_and_jumpers
				--	LIBS:bel_primitives
				--	LIBS:bel_stm32
				--	LIBS:nucleo_core-cache
				--	EELAYER 25 0
				--	EELAYER END

				-- This data goes into a the sheet_header. When the schematic file has been
				-- read completely, the sheet_header is appended to global list_of_sheet_headers. 
				-- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.
				-- NOTE: The library entries in the header are not used by kicad. However, they must be read
				-- and stored in sheet_header.libraries.
								
				use type_lines;
			
			begin -- make_sheet_header
				line_cursor := type_lines.first (lines);
				while line_cursor /= type_lines.no_element loop

					--log ("---> C " & to_string (line));
					
					-- Field #1 of the line must be broken down by its own ifs in order to get "LIBS" and "bel_stm32"
					if get_field_from_line (field (et_kicad.line,1), 1, latin_1.colon) = schematic_library then

						-- for the log: write library name
						log ("uses library " & get_field_from_line (field (et_kicad.line,1), 2, latin_1.colon),
							log_threshold + 1);

						-- Store bare library name in the list sheet_header.libraries:
						-- We use a doubly linked list because the order of the library names must be kept.
						et_libraries.type_library_names.append (
							container => sheet_header.libraries,
							new_item => et_libraries.type_library_name.to_bounded_string (
								get_field_from_line (field (et_kicad.line,1), 2, latin_1.colon))
							);

					end if;

					-- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
					-- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
					if field (et_kicad.line,1) = schematic_eelayer then
						if field (et_kicad.line,2) = schematic_eelayer_end then
							null;
						else
							-- append layer numbers to the sheet header
							sheet_header.eelayer_a := positive'value(
								field (et_kicad.line,2));

							sheet_header.eelayer_b := natural'value(
								field (et_kicad.line,3));
						end if;
					end if;

					next (line_cursor);
				end loop;

				-- Add sheet_header to module.
                -- NOTE: The file name serves as key in order to match from file to header.
				add_sheet_header (
					header => sheet_header,
					sheet => current_schematic);

			end make_sheet_header;

			procedure make_drawing_frame (
			-- Builds the drawing frame.
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
				
				-- These are the components of the drawing frame. At the end
				-- of this procedure they are assembled to a drawing frame:
				
				frame : et_schematic.type_frame; -- a single drawing frame
				title_block_text 	: et_schematic.type_title_block_text; -- a single text within the title block
				title_block_texts 	: et_schematic.type_title_block_texts.list; -- a list of title block texts
				title_block 		: et_schematic.type_title_block; -- a full title block
				
				-- If the description reveals there is more than one sheet, we have a hierarchic design. Means we
				-- need to read follwing sheet sections.
				-- The sheet_number_current obtained here serves as part of the coordinates of objects found on this sheet.
				-- The sheet description looks like this:

				-- $Descr A4 11693 8268
				-- encoding utf-8
				-- Sheet 5 8
				-- Title ""
				-- Date ""
				-- Rev ""
				-- Comp ""
				-- Comment1 ""
				-- Comment2 ""
				-- Comment3 ""
				-- Comment4 ""
				-- $EndDescr
				
				use type_lines;
			
			begin -- make_drawing_frame
				log ("making drawing frame ...", log_threshold);
				log_indentation_up;
			
				line_cursor := type_lines.first (lines);

				-- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
				-- CS test field count				
				frame.paper_size	:= type_paper_size'value (field (et_kicad.line,2));
				frame.size_x		:= mil_to_distance (field (et_kicad.line,3));
				frame.size_y 		:= mil_to_distance (field (et_kicad.line,4)); 
				
				--frame.coordinates.path := path_to_submodule;
				set_path (frame.coordinates, path_to_submodule);

				-- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
				-- kicad built-in things and remain unassigned here.

				next (line_cursor);

				-- read endcoding from a line like "encoding utf-8"
				-- CS: checks only for a non-default endcoding and outputs a warning.
				-- CS: we assume only one encoding. other encodings are ignored currently.
				-- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
				-- good idea.
				if field (et_kicad.line,1) = schematic_keyword_encoding then
					-- CS test field count
					if field (et_kicad.line,2) /= encoding_default then
						log (message_warning & "non-default endcoding '" & field (et_kicad.line,2) & "' found !");
					end if;
				end if;

				next (line_cursor);

				-- read sheet number from a line like "Sheet 1 7"
				if field (et_kicad.line,1) = schematic_keyword_sheet then
					-- CS test field count					
					sheet_number_current := to_sheet_number (field (et_kicad.line,2));
					log ("sheet" & to_string (sheet_number_current) & " ...", log_threshold + 1);
					sheet_count_total := to_sheet_number (field (et_kicad.line,3));
					-- CS: sheet_count_total must not change from sheet to sheet. Check required.
					if sheet_count_total > 1 then
						-- Set in the list_of_submodules (to be returned) the parent_module. The schematic file 
						-- being processed (see input parameters of read_schematic) becomes the parent module
						-- of the submodules here.
						list_of_submodules.parent_module := type_submodule_name.to_bounded_string (to_string (current_schematic));
					end if;
					-- CS: make sure total sheet count is less or equal current sheet number.

					-- Our temporarily drawing frame gets the current sheet number assigned.
					set_sheet (frame.coordinates, sheet_number_current);
				end if;						

				next (line_cursor);
				
				-- read sheet title from a line like "Title "abc""
				if field (et_kicad.line,1) = schematic_keyword_title then                        
					-- CS test field count					
					title_block_text.meaning := TITLE;
					title_block_text.text := type_title_block_text_string.to_bounded_string((field (et_kicad.line,2)));
					type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);
				
				-- read date from a line like "Date "1981-01-23""
				if field (et_kicad.line,1) = schematic_keyword_date then                        
					-- CS test field count					
					title_block_text.meaning := DRAWN_DATE;
					title_block_text.text := type_title_block_text_string.to_bounded_string((field (et_kicad.line,2)));
					type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);
				
				-- read revision from a line like "Rev "9.7.1"
				if field (et_kicad.line,1) = schematic_keyword_revision then                        
					-- CS test field count					
					title_block_text.meaning := REVISION;
					title_block_text.text := type_title_block_text_string.to_bounded_string ((field (line,2)));
					type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);

				-- read company name
				if field (et_kicad.line,1) = schematic_keyword_company then
					-- CS test field count					
					title_block_text.meaning := COMPANY;
					title_block_text.text := type_title_block_text_string.to_bounded_string((field (et_kicad.line,2)));
					type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);

				-- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
				if  field (et_kicad.line,1) = schematic_keyword_comment_1 or
					field (et_kicad.line,1) = schematic_keyword_comment_2 or
					field (et_kicad.line,1) = schematic_keyword_comment_3 or 
					field (et_kicad.line,1) = schematic_keyword_comment_4 then
					-- CS test field count
					title_block_text.meaning := MISC;
					title_block_text.text := type_title_block_text_string.to_bounded_string ((field (et_kicad.line,2)));
					type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				-- FINALIZE

				-- Make temporarily title_block complete by assigning coordinates and list of texts.
				-- Then purge temporarily list of texts.
				-- Then append temporarily title block to main module.
										
				set_path (title_block.coordinates, path_to_submodule);
				
				title_block.texts := title_block_texts; -- assign collected texts list to temporarily title block
				-- CS: x/y coordinates and list of lines are kicad built-in things and thus not available currently.

				-- purge temporarily texts
				type_title_block_texts.clear (title_block_texts);

				-- append title block to module
				add_title_block (title_block);
				
				-- append temporarily drawing frame to module
				add_frame (frame);

				log_indentation_down;

				exception
					when event:
						others =>
							log_indentation_reset;
							--log (message_error , console => true);
							log (ada.exceptions.exception_message (event));
							raise;
				
			end make_drawing_frame;


			procedure make_gui_sheet (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds the GUI sheet.
				sheet : et_schematic.type_gui_submodule; -- the hierarchical GUI sheet being built
				name, file : et_coordinates.type_submodule_name.bounded_string; -- sheet name and file name (must be equal)

				port_inserted : boolean; -- used to detect multiple ports with the same name
				port_cursor : type_gui_submodule_ports.cursor; -- obligatory, but not read

				use type_lines;
				use et_coordinates.type_submodule_name;
				use et_libraries;
				use et_configuration;

				text_size : type_text_size; -- temporarily storage of a text size before being checked
			
				function to_direction (dir_in : in string) return type_port_direction is
				-- Converts a string to type_port_direction.
					result : type_port_direction;
					dir : type_sheet_port_direction; -- see et_kicad.ads
				begin
					dir := type_sheet_port_direction'value (dir_in);
					
					case dir is
						when I => result := INPUT;
						when O => result := OUTPUT;
						when B => result := BIDIR;
						when T => result := TRISTATE;
						when U => result := PASSIVE;
					end case;

					log_indentation_up;
					log (to_string (result), log_threshold + 2);
					log_indentation_down;
					
					return result;

					exception
						when constraint_error =>
							log_indentation_reset;
							log (message_error & "invalid port direction '" 
								 & dir_in & "' !");
							-- CS: provide more details
							raise;

				end to_direction;

				function to_orientation (or_in : in string) return et_coordinates.type_angle is
				-- Converts a string to type_angle
					result : et_coordinates.type_angle;
					orientation : type_sheet_port_orientation; -- see et_kicad.ads
				begin
					orientation := type_sheet_port_orientation'value (or_in);

					case orientation is
						when R => result := et_coordinates.type_angle (0.0);
						when L => result := et_coordinates.type_angle (180.0);
					end case;
					
					return result;

					exception
						when constraint_error =>
							log_indentation_reset;
							log (message_error & "invalid port orientation '" 
								 & or_in & "' !");
							-- CS: provide more details
							raise;

				end to_orientation;
					
			begin -- make_gui_sheet
				log ("making gui sheet ...", log_threshold);
				log_indentation_up;
				
				line_cursor := type_lines.first (lines);
-- 				log (to_string (et_kicad.line), log_threshold + 1);

				-- read GUI sheet position and size from a line like "S 4050 5750 1050 650"
				if field (et_kicad.line,1) = schematic_keyword_sheet_pos_and_size then
					-- CS test field count
					set_path (sheet.coordinates, path_to_submodule);
					--log ("path " & to_string (path (sheet.coordinates)));
					set_sheet (sheet.coordinates, sheet_number_current);
					set_x (sheet.coordinates, mil_to_distance (field (et_kicad.line,2)));
					set_y (sheet.coordinates, mil_to_distance (field (et_kicad.line,3)));

					sheet.size_x := mil_to_distance (field (et_kicad.line,4));
					sheet.size_y := mil_to_distance (field (et_kicad.line,5));                                
				end if;

				next (line_cursor);
-- 				log (to_string (et_kicad.line), log_threshold + 1);
				
				-- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
				if field (et_kicad.line,1) = schematic_keyword_sheet_timestamp then 
					-- CS test field count					
					sheet.timestamp := type_timestamp (field (et_kicad.line,2));
				end if;

				next (line_cursor);
				
				-- Read sheet name from a line like "F0 "mcu_stm32f030" 60"
				-- Since this is the black-box-representation of a kicad-sheet its name is threated as name of a submodule.
				-- The sheet name will later be compared with the sheet file name.
				if field (et_kicad.line,1) = schematic_keyword_sheet_name then
					-- CS test field count					
					name := to_submodule_name (field (et_kicad.line,2));

					-- set text size of sheet name and test for excessive text size.
					sheet.text_size_of_name := to_text_size (mil_to_distance (field (et_kicad.line,3)));

					-- Test text size by category.
					check_schematic_text_size (category => SHEET_NAME, size => sheet.text_size_of_name);
				end if;

				next (line_cursor);
				
				-- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
				if field (et_kicad.line,1) = schematic_keyword_sheet_file then
					-- CS test field count					
					file := to_submodule_name (submodule => base_name (field (et_kicad.line,2)));

					-- set text size of file name and test for excessive text size
					sheet.text_size_of_file := to_text_size (mil_to_distance (field (et_kicad.line,3)));

					-- Test text size by category.
					check_schematic_text_size (category => FILE_NAME, size => sheet.text_size_of_file);
					
					-- Test if sheet name and file name match:
					if name /= file then
						log_indentation_reset;
						log (text => message_error & "name mismatch: sheet name '" 
							& to_string (submodule => name) & "' differs from file name '"
							& to_string (submodule => file));
						raise constraint_error;
					end if;

					-- append sheet file name (with extension) to list_of_submodules to be returned to parent unit
					type_submodule_names.append (
						list_of_submodules.list,
						to_submodule_name (submodule => field (et_kicad.line,2)));
				end if;

				log ("hierarchic sheet " & to_string (submodule => name), log_threshold + 1);
				
				-- Read sheet ports from a line like "F2 "SENSOR_GND" I R 2250 3100 60".
				-- The index after the F is a successive number that increments on every port:
				-- So the next port would be "F3 "SENSOR_VCC" I R 2250 3300 60" ...
				next (line_cursor);

				-- Read ports of hierachic sheet if any. Otherwise output a warning.
				-- If no ports available, the line cursor points to a no_element.
				if line_cursor /= no_element then
					
					-- Test of excessive text size.
					text_size := to_text_size (mil_to_distance (field (et_kicad.line, 7)));

					-- Test text size by category.
					check_schematic_text_size (category => PORT_NAME, size => text_size);
					
					while line_cursor /= no_element loop
						log_indentation_up;
						log ("port " & strip_quotes (field (et_kicad.line, 2)), log_threshold + 2);

						-- add port
						type_gui_submodule_ports.insert (
							container => sheet.ports,
							key => to_net_name (field (et_kicad.line, 2)), -- port name
							new_item => (
								direction 	=> to_direction (field (et_kicad.line, 3)),
								orientation	=> to_orientation (field (et_kicad.line, 4)),
								coordinates	=> to_point (field (et_kicad.line, 5), field (et_kicad.line, 6)),
								text_size	=> text_size,
								processed	=> false),
							inserted => port_inserted,
							position => port_cursor
							);

						-- if port could not be inserted -> abort
						if not port_inserted then
							log_indentation_reset;
							log (message_error & "multiple usage of port " & field (et_kicad.line, 2) & " !");
							raise constraint_error;
						end if;
						
						log_indentation_down;
						next (line_cursor);
					end loop;

				else -- sheet has no ports -> warning
					log (message_warning & "hierarchic sheet '" & to_string (submodule => name) & "' has no ports !");
				end if;

				-- insert the hierarchical GUI sheet in module (see et_schematic type_module)
				add_gui_submodule (name, sheet);

				log_indentation_down;
				
				exception
					when event:
						others =>
							log_indentation_reset;
							--log (message_error , console => true);
							log (ada.exceptions.exception_message (event));
							raise;

			end make_gui_sheet;


			function net_segment_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a net segment header like "Wire Wire Line"
				result : boolean := false;
			begin
				-- CS test field count
				if field (line,1) = schematic_keyword_wire then
					if field (line,2) = schematic_keyword_wire then
						if field (line,3) = schematic_keyword_line then
							result := true;
						end if;
					end if;
				end if;
				return result;
			end net_segment_header;
			
			procedure make_net_segment (
				lines			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a net segment and appends it to the collection of wild segments.

				-- After the segment heaser "Wire Wire Line" the next line like
				-- "2250 3100 2400 3100" is read here. It contains start and end points 
				-- of a net segment.
			
				segment : type_wild_net_segment; -- the segment being built
			begin
				log ("making net segment ...", log_threshold);
				log_indentation_up;

				line_cursor := type_lines.first (lines);
				
				-- Build a temporarily net segment with fully specified coordinates:
				set_path (segment.coordinates_start, path_to_submodule);
				set_path (segment.coordinates_end, path_to_submodule);
				
				-- The sheet number. NOTE: Kicad V4 can handle only one sheet per submodule. The sheet numbering is consecutive and does
				-- not care about the actual submodule names.
				set_sheet (segment.coordinates_start, sheet_number_current);
				set_sheet (segment.coordinates_end,   sheet_number_current);

				-- the x/y position
				set_x (segment.coordinates_start, mil_to_distance (field (et_kicad.line,1)));
				set_y (segment.coordinates_start, mil_to_distance (field (et_kicad.line,2)));
				set_x (segment.coordinates_end, mil_to_distance (field (et_kicad.line,3)));
				set_y (segment.coordinates_end, mil_to_distance (field (et_kicad.line,4)));

				-- Ignore net segments with zero length (CS: for some reason they may exist. could be a kicad bug)
				-- If a net segment has zero length, issue a warning.
				if length (segment) > zero_distance then 

					-- The net segments are to be collected in a wild list of segments for later sorting.
					log (to_string (segment => segment, scope => xy), log_threshold + 1);
					
					type_wild_segments.append (wild_segments, segment);
					
				else -- segment has zero length
					log (message_warning & affected_line (et_kicad.line) & "Net segment with zero length found -> ignored !");
				end if; -- length

				log_indentation_down;
			end make_net_segment;

			function junction_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a net junction "Connection ~ 4650 4600"
				result : boolean := false;
			begin
				-- CS test field count
				if field (line,1) = schematic_keyword_connection then
					if field (line,2) = schematic_tilde then
						result := true;
					end if;
				end if;
				return result;
			end junction_header;

			procedure make_junction (
				line			: in type_fields_of_line;
				log_threshold	: in type_log_level) is
			-- Builds a net junction and stores it both in the 
			-- junction list of the module (for statistics, ERC, ...) 
			-- AND in the wild list junctions.
			-- The wild list is needed when the anonymous strands of
			-- the sheet are built (see procedure build_anonymous_strands).
			-- The wild list contains the junction of the current sheet exclusively.
				junction : et_schematic.type_net_junction;  -- the junction being built

				procedure append_junction (
				-- add junction to module.junctions
					module_name : in type_submodule_name.bounded_string;
					module		: in out type_module) is
				begin
					type_junctions.append (
						container => module.junctions,
						new_item => junction);
				end append_junction;
				
			begin -- make_junction
				log ("making net junction ...", log_threshold);
				log_indentation_up;
				
				set_path (junction.coordinates, path_to_submodule);
				set_sheet (junction.coordinates, sheet_number_current);
				set_x (junction.coordinates, mil_to_distance (field (line,3)));
				set_y (junction.coordinates, mil_to_distance (field (line,4)));

				-- for the log
				log (to_string (junction => junction, scope => xy));

				-- add to wild list of junctions
				type_junctions.append (wild_junctions, junction);

				-- add to module.junctions
				type_rig.update_element (
					container => rig,
					position => module_cursor,
					process => append_junction'access);

				log_indentation_down;
			end make_junction;

			function simple_label_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a simple label like 
			-- "Text Label 2350 3250 0 60 ~ 0"
				result : boolean := false;
			begin
				-- CS test field count
				if 	field (line,1) = schematic_keyword_text and 
					field (line,2) = schematic_keyword_label_simple then
						result := true;
				end if;
				return result;
			end simple_label_header;

			procedure make_simple_label (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a simple net label and appends it to the collection of wild simple labels.

				-- The label header "Text Label 2350 3250 0 60 ~ 0" and the next line like
				-- "net_name_abc" is read here. It contains the supposed net name.

				use et_configuration;
				
				label : type_net_label_simple; -- the label being built
			begin
				log ("making simple label ...", log_threshold);
				log_indentation_up;
				
				line_cursor := type_lines.first (lines);

				-- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
				set_path (label.coordinates, path_to_submodule);
				set_sheet (label.coordinates, sheet_number_current);
				set_x (label.coordinates, mil_to_distance (field (et_kicad.line,3)));
				set_y (label.coordinates, mil_to_distance (field (et_kicad.line,4)));

				label.orientation := to_angle (field (et_kicad.line,5));
				label.size := mil_to_distance (field (et_kicad.line,6));
				label.style := to_text_style (style_in => field (et_kicad.line,7), text => true);
				label.width := et_libraries.type_text_line_width'value (field (et_kicad.line,8));

				next (line_cursor);

				-- Make sure the label text (later this will be a net name) is not longer
				-- than allowed.
				check_net_name_length (field (et_kicad.line,1));
				
				-- get label text and put it to temporarily simple label
				label.text := type_net_name.to_bounded_string (field (et_kicad.line,1));

				-- Make sure there are no forbidden characters in the net name.
				check_net_name_characters (label.text);
				
				-- for the log
				log (to_string (label => type_net_label (label), scope => xy));

				check_schematic_text_size (category => net_label, size => label.size);
				-- CS: check label style
				-- CS: check label line width
				
				-- The simple labels are to be collected in a wild list of simple labels.
				type_simple_labels.append (wild_simple_labels, label);

				log_indentation_down;
			end make_simple_label;

			function tag_label_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a global or hierarchic label like 
			-- "Text HLabel 2700 2000 0 60 Input ~ 0" or
			-- "Text GLabel 4700 3200 1 60 UnSpc ~ 0"
				result : boolean := false;
			begin
				-- CS test field count
				if field (line,1) = schematic_keyword_text and 
					(field (line,2) = schematic_keyword_label_hierarchic or
					 field (line,2) = schematic_keyword_label_global) then
						result := true;
				end if;
				return result;
			end tag_label_header;

			procedure make_tag_label (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a global or hierachical label and appends it to the collection of wild tag labels.

				-- The label header "Text GLabel 4700 3200 1 60 UnSpc ~ 0" and the next line like
				-- "net_name_abc" is read here. It contains the supposed net name.

				use et_configuration;
				
				label : type_net_label_tag; -- the label being built
			begin
				log ("making tag label ...", log_threshold);
				log_indentation_up;

				line_cursor := type_lines.first (lines);

				-- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
				-- The keyword in field 2 tells whether we have a hierarchic or global label:
				if field (et_kicad.line,2) = schematic_keyword_label_hierarchic then
					label.hierarchic := true;
					label.global := false;
				else
					label.hierarchic := false;
					label.global := true;
				end if;

				set_path (label.coordinates, path_to_submodule);
				set_sheet (label.coordinates, sheet_number_current);
				set_x (label.coordinates, mil_to_distance (field (et_kicad.line,3)));
				set_y (label.coordinates, mil_to_distance (field (et_kicad.line,4)));

				label.orientation := to_angle (field (et_kicad.line,5));
				label.direction := to_direction (field (et_kicad.line,7));

				-- build text attributes from size, font and line width
				label.size := mil_to_distance (field (et_kicad.line,6));
				label.style := to_text_style (style_in => field (et_kicad.line,8), text => true);
				label.width := et_libraries.type_text_line_width'value (field (et_kicad.line,9));

				next (line_cursor);

				-- Make sure the label text (later this will be a net name) is not longer
				-- than allowed.
				check_net_name_length (field (et_kicad.line,1));
				
				-- get label text
				label.text := type_net_name.to_bounded_string (field (et_kicad.line,1));
				
				-- Make sure there are no forbidden characters in the net name.
				check_net_name_characters (label.text);

				-- for the log
				log (to_string (label => type_net_label (label), scope => xy));

				check_schematic_text_size (category => net_label, size => label.size);
				-- CS: check style and line width
				
				-- The tag labels are to be collected in a wild list of tag labels for later sorting.
				type_tag_labels.append (wild_tag_labels, label);

				log_indentation_down;
			end make_tag_label;

			function text_note_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a text note like
			-- Text Notes 7100 6700 0 67 Italic 13
			-- ET Test Circuit
				result : boolean := false;
			begin
				-- CS test field count
				if field (line,1) = schematic_keyword_text and 
					field (line,2) = schematic_keyword_note then
						result := true;
				end if;
				return result;
			end text_note_header;

			procedure make_text_note (
				lines			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a text note and appends it to the collection of text notes.

				-- The label header "Text Notes 3400 2800 0 60 Italic 12" and the next line like
				-- "ERC32 Test Board" is read here. It contains the actual text.
			
				note : type_note; -- the text note being built
			begin
				log ("making text note ...", log_threshold);
				log_indentation_up;
				
				line_cursor := type_lines.first (lines);

				-- set coordinates
				set_path (note.coordinates, path_to_submodule);
				set_sheet (note.coordinates, sheet_number_current);
				set_x (note.coordinates, mil_to_distance (field (et_kicad.line,3)));
				set_y (note.coordinates, mil_to_distance (field (et_kicad.line,4)));
				
				note.orientation   := to_angle (field (et_kicad.line,5));

				-- set text size and check for excessive size
				note.size := et_libraries.to_text_size (mil_to_distance (field (et_kicad.line,6)));
				
				note.style := to_text_style (style_in => field (et_kicad.line,7), text => true);
				note.line_width := mil_to_distance (field (et_kicad.line,8));

				next (line_cursor);

				-- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
				-- CS: store lines in a list of lines instead ?
				-- CS: Currently we store the line as it is in tmp_note.text
				note.content := et_libraries.type_text_content.to_bounded_string (to_string (line));

				write_note_properties (note, log_threshold + 1);
				
				-- the notes are to be collected in the list of notes
				add_note (note);

				log_indentation_down;
			end make_text_note;

			function component_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a component.
			begin
				-- CS test field count
				if field (line,1) = schematic_component_header then
					return true;
				else 
					return false;
				end if;
			end component_header;

			function component_footer (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a footer of a component.
			begin
				-- CS test field count
				if field (line,1) = schematic_component_footer then
					return true;
				else 
					return false;
				end if;
			end component_footer;

			
			procedure make_component (lines : in type_lines.list) is
			-- Builds a unit or a component and inserts it in the component list of 
			-- current module. The information required to make a component is provided
			-- in parameter "lines".

			-- Some entries of the component section are relevant for the whole component.
			-- Some entries are unit specific.
			-- The component section looks like this example:
			
			-- L 74LS00 U1		-- component specific
			-- U 4 1 5965E676	-- unit specific
			-- P 4100 4000		-- unit specific
			-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
			-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
			-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
			-- F 3 "" H 4100 4000 50  0001 C CNN
			-- 	4    4100 4000		-- same as x/y pos

			--  1    0    0  -1  -- orientation 0,   mirror normal
			--  0   -1   -1   0  -- orientation 90,  mirror normal
			-- -1    0    0   1  -- orientation 180, mirror normal 
			-- 	0    1    1   0  -- orientation -90, mirror normal  

			-- 	1    0    0   1  -- orientation 0,   mirror --
			--  0   -1    1   0  -- orientation 90,  mirror -- 
			-- -1    0    0  -1  -- orientation 180, mirror -- 
			--  0    1   -1   0  -- orientation -90, mirror -- 

			-- -1    0    0  -1  -- orientation 0,   mirror |
			--  0    1   -1   0  -- orientation 90,  mirror |
			--  1    0    0   1  -- orientation 180, mirror |
			--  1    0    0   1  -- orientation -90, mirror |

				use et_libraries;
				use et_string_processing;

				reference					: type_component_reference;	-- like IC5	
				appearance					: type_component_appearance := et_libraries.sch; -- CS: why this default ?
				generic_name_in_lbr			: type_component_generic_name.bounded_string; -- like TRANSISTOR_PNP
				unit_name					: type_unit_name.bounded_string; -- A, B, PWR, CT, IO-BANK1 ...
				position					: et_coordinates.type_coordinates;
				orientation					: et_coordinates.type_angle;
				mirror						: type_mirror;
				timestamp					: type_timestamp;
				alternative_representation	: et_schematic.type_alternative_representation;
			
				-- These are the "field found flags". They signal if a particular text field has been found.
				-- They are evaluated once the given lines are read completely.
				field_reference_found		: boolean := false;
				field_value_found			: boolean := false;
				field_commissioned_found	: boolean := false;
				field_updated_found			: boolean := false;
				field_author_found			: boolean := false;
				field_package_found			: boolean := false;
				field_datasheet_found		: boolean := false;
				field_purpose_found			: boolean := false;
				field_partcode_found		: boolean := false;
				field_bom_found				: boolean := false;

				-- These are the actual fields that descibe the component more detailled.
				-- They are contextual validated once the given lines are read completely.
				field_reference		: type_text (meaning => et_libraries.reference); -- like IC5 (redundant information with referenc, see above)
				field_value			: type_text (meaning => value);	-- like 74LS00
				field_commissioned 	: type_text (meaning => commissioned); -- 2018-01-04T03:56:09
				field_updated		: type_text (meaning => updated); -- 2018-02-04T03:56:09
				field_author		: type_text (meaning => author); -- like Steve Miller
				field_package		: type_text (meaning => packge); -- like "bel_primiteves:S_SOT23"
				field_datasheet		: type_text (meaning => datasheet); -- might be useful for some special components
				field_purpose		: type_text (meaning => purpose); -- to be filled in schematic later by the user
				field_partcode		: type_text (meaning => partcode); -- like "R_PAC_S_0805_VAL_"
				field_bom			: type_text (meaning => bom); -- yes/no
			
				function to_field return et_libraries.type_text is
				-- Converts a field like "F 1 "green" H 2700 2750 50  0000 C CNN" to a type_text
					function field (line : in type_fields_of_line; position : in positive) return string renames get_field_from_line;
					text_position : type_2d_point;

					size : type_text_size;
				begin
					-- test if the field content is longer than allowed:
					check_text_content_length (field (et_kicad.line,3));
					
					set_x (text_position, mil_to_distance (field (et_kicad.line,5)));
					set_y (text_position, mil_to_distance (field (et_kicad.line,6)));

					size := mil_to_distance (field (et_kicad.line,7));

					return (
						-- read text field meaning
						meaning 	=> to_text_meaning (line => et_kicad.line, schematic => true),

						-- read content like "N701" or "NetChanger" from field position 3
						content		=> type_text_content.to_bounded_string (field (et_kicad.line,3)),

						-- read orientation like "H"
						orientation	=> to_field_orientation (field (et_kicad.line,4)),

						-- read coordinates
						position	=> text_position,
										
						size		=> size,
						style		=> to_text_style (style_in => field (et_kicad.line,10), text => false),
						line_width	=> type_text_line_width'first,

						-- build text visibility
						visible		=> to_field_visible (
											vis_in		=> field (et_kicad.line,8),
											schematic	=> true),

						-- build text alignment
						alignment	=> (
										horizontal	=> to_alignment_horizontal (field (et_kicad.line,9)),
										vertical	=> to_alignment_vertical   (field (et_kicad.line,10)))
						);
				end to_field;

				procedure check_text_fields (log_threshold : in type_log_level) is 
				-- Tests if any "field found" flag is still cleared and raises an alarm in that case.
				-- Perfoms a CONTEXTUAL VALIDATION of the text fields before they are used to 
				-- assemble and insert the component into the component list of the module.

					use et_configuration;
				
					procedure missing_field (m : in et_libraries.type_text_meaning) is 
					begin
						log_indentation_reset;
						log (
							text => message_error 
								& "component " & to_string (reference) 
								& latin_1.space
								& to_string (position => position)
								& latin_1.lf
								& "text field " & et_libraries.to_string (m) & " missing !",
							console => true);
						
						raise constraint_error;
					end missing_field;

					commissioned, updated : et_string_processing.type_date;

				begin -- check_text_fields
					log_indentation_up;

					-- write precheck preamble
					log ("component " 
						& to_string (reference) & " prechecking fields ...", level => log_threshold);
					log_indentation_up;
					
					-- reference
					-- NOTE: the reference prefix has been checked already in main of procedure make_component
					log ("reference", level => log_threshold + 1);
					if not field_reference_found then
						missing_field (et_libraries.reference);
						-- CS: use missing_field (text_reference.meaning); -- apply this to other calls of missing_field too
					else
						-- verify text_reference equals reference. @kicad: why this redundance ?
						-- KiCad stores redundant information on the component reference as in this example;

						-- $Comp
						-- L 74LS00 IC1 <- reference
						-- U 1 1 59969711
						-- P 4100 4000
						-- F 0 "IC1" H 4100 4050 50  0000 C BIB <- text_reference
						
						if et_libraries.to_string (reference) /= et_libraries.content (field_reference) then
							log_indentation_reset;
							log (message_error & "reference mismatch ! Header reads " 
								 & et_libraries.to_string (reference) & " but field contains " 
								 & et_libraries.content (field_reference),
								console => true);
							raise constraint_error;
						end if;

						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
					end if;

					-- value
					log ("value", level => log_threshold + 1);
					if not field_value_found then
						missing_field (et_libraries.value);
					else
						-- depending on the component reference (like R12 or C9) the value must meet certain conventions:
						validate_component_value (
														 
							-- the content of the value field like 200R or 10u
							value => type_component_value.to_bounded_string (content (field_value)), 
								
							-- the component reference such as R4 or IC34
							reference => reference,

							appearance => appearance);

						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_value.size);
					end if;

					-- commissioned
					log ("commissioned", level => log_threshold + 1);
					if not field_commissioned_found then
						missing_field (et_libraries.commissioned);
					else
						-- The commissioned time must be checked for plausibility and syntax.
						-- The string length is indirecty checked on converting the field content to derived type_date.
						commissioned := et_string_processing.type_date (et_libraries.content (field_commissioned));
						if not et_string_processing.date_valid (commissioned) 
							then raise constraint_error;
						end if;
						
						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_commissioned.size);
					end if;

					-- updated
					log ("updated", level => log_threshold + 1);
					if not field_updated_found then
						missing_field (et_libraries.updated);
					else
						-- The update time must be checked for plausibility and syntax.
						-- The string length is indirecty checked on converting the field content to derived type_date.					
						updated := et_string_processing.type_date (et_libraries.content (field_updated));
						if not et_string_processing.date_valid (updated) 
							then raise constraint_error;
						end if;

						-- The update must be later than the commission date:
						if compare_date (
							left => type_component_date (content (field_updated)),
							right => type_component_date (content (field_commissioned))) then
							log (message_warning & "commission date must be before update !");
							-- CS: show reference, commission and update time
						end if;

						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_updated.size);
					end if;

					-- author
					log ("author", level => log_threshold + 1);
					if not field_author_found then
						missing_field (et_libraries.author);
					else
						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_author.size);
						-- CS: check content of text_author
					end if;

					-- If we are checking fields of a real component there are more 
					-- fields to be checked. If it is about a virtual component, those 
					-- fields are ignored and thus NOT checked:
					case appearance is
						when sch_pcb =>
								
							-- package
							log ("package/footprint", level => log_threshold + 1);
							if not field_package_found then
								missing_field (et_libraries.packge);
							else
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_package.size);
								-- CS: check content of field_package
								-- use library_name (content (field_package))
								-- check/validate library name (length, characters, ...)
								-- make sure the library exists. mind search order of footprint libraries

								-- check/validate package name (length, characters, ...)
								check_package_name_length (to_string (package_name (content (field_package))));
								check_package_name_characters (package_name (content (field_package)));
							end if;

							-- datasheet
							log ("datasheet", level => log_threshold + 1);
							if not field_datasheet_found then
								missing_field (et_libraries.datasheet);
							else
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_datasheet.size);
								-- CS: check content of field_datasheet
							end if;

							-- partcode
							log ("partcode", level => log_threshold + 1);
							if not field_partcode_found then
								missing_field (et_libraries.partcode);
							else
								log_indentation_up;
							
								validate_component_partcode_in_schematic (
									-- the content of the partcode field like R_PAC_S_0805_VAL_100R
									partcode => type_component_partcode.to_bounded_string (content (field_partcode)),

									-- the component reference like R45
									reference => reference,

									-- The component package name like S_0805 must be extracted from the field field_package.
									-- The field contains something like bel_ic:S_SO14 . 
									-- The part after the colon is the package name. The part before the colon is the library
									-- name which is not of interest here.
									packge => type_component_package_name.to_bounded_string (field (
										line => read_line ( -- CS use package_name (content (field_package)
											line => content (field_package), -- bel_ic:S_SO14
											ifs => latin_1.colon),
										position => 2)), -- the block after the colon

									-- the content of the value field like 200R or 10u
									value => type_component_value.to_bounded_string (content (field_value)),

									-- the BOM status
									bom => type_bom'value (content (field_bom)),

									log_threshold => log_threshold + 1
									);

									check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_partcode.size);

								log_indentation_down;
							end if;
							
							-- purpose
							log ("purpose", level => log_threshold + 1);
							if not field_purpose_found then
								missing_field (et_libraries.purpose);
							else
								-- A more detailled test of the purpose can be performed if component prefixes
								-- are specified in the configuration file:
								if et_configuration.component_prefixes_specified then
								
									-- Depending on the status of operator interaction:
									case et_configuration.requires_operator_interaction (reference.prefix) is

										-- Even if NO INTERACTION is required, we expect at least 
										-- the purpose_default string.
										when et_configuration.NO =>
											if content (field_purpose) /= purpose_default then
												log (message_warning & "expected default " & purpose_default & " !");
											end if;
											
										-- If operator INTERACTION IS REQUIRED, we expect something useful in 
										-- the purpose field. 
										-- Furhter-on there must be NO other component already of this category with 
										-- the same purpose. Example: It is forbidden to have 
										-- an X1 and an X2 both with purpose "PWR_IN"
										when et_configuration.YES =>
											validate_purpose (content (field_purpose)); -- must be something useful

											-- test if purpose already used for this category 
											if et_configuration.multiple_purpose (
												category => et_configuration.category (reference), -- derive cat from reference
												purpose => to_purpose (content (field_purpose)),
												log_threshold => log_threshold + 2) > 0 then

													-- purpose already in use -> error
													et_configuration.multiple_purpose_error (
														category => et_configuration.category (reference),
														purpose => to_purpose (content (field_purpose)),
														log_threshold => log_threshold + 2);
											end if;

											-- make sure the purpose text is visible in the graphical representation:
											if field_purpose.visible = no then
												log_indentation_reset;
												log (message_error & "component " 
													& et_libraries.to_string (reference)
													& " purpose not visible !",
													console => true);
												raise constraint_error;
											end if;
											
									end case;

								end if;
									
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_purpose.size);
							end if;

							-- bom
							log ("bom", level => log_threshold + 1);
							if not field_bom_found then
								missing_field (et_libraries.bom);
							else
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_bom.size);
								-- CS: check content of field_bom
							end if;
							
							-- put_line (indent(indentation + 1) & "crosschecks");
							-- CS: test partcode, verify agsinst prefix, value and package
							-- CS: test function against prefix of user interactive parts (X, SW, LED, ...)

							
						when others => null; -- CS ?
					end case;

					log_indentation_down;
					log_indentation_down;
		-- 			log_indentation_down;				
					
					exception
						when constraint_error =>
							log_indentation_reset;
							log (
								text => message_error & "invalid field in component " & et_libraries.to_string (reference)
									& " at " & to_string (position => position),
								console => true);
							-- CS: evaluate prog position and provided more detailled output
							raise constraint_error;

				end check_text_fields;


				function generic_name_to_library (
				-- Returns the full name of the library where given generic component is contained.
				-- The given reference serves to provide a helpful error message on the affected 
				-- component in the schematic.
					component : in type_component_generic_name.bounded_string; -- the generic name like "RESISTOR"
					reference : in type_component_reference) -- the reference in the schematic like "R4"
					return type_full_library_name.bounded_string is -- the full library name like "../libraries/resistors.lib"

					use type_libraries;
					use type_full_library_name;
				
					component_found : boolean := false; -- goes true once the given component was found in any library
					
					lib_cursor : type_libraries.cursor := component_libraries.first; -- points to the library being searched in
					library : type_full_library_name.bounded_string; -- the full library name to be returned

					procedure query_components (
					-- Queries the components in the current library. Exits prematurely once the 
					-- given generic component was found.
						lib_name : in type_full_library_name.bounded_string;
						components : in et_libraries.type_components.map) is
						use et_libraries.type_components;
						component_cursor : et_libraries.type_components.cursor := components.first;
						use et_libraries.type_component_generic_name;
					begin
						log_indentation_up;
						while component_cursor /= et_libraries.type_components.no_element loop
							
							log (et_libraries.to_string (key (component_cursor)), log_threshold + 4);

							-- Sometimes generic names in the library start with a tilde. it must
							-- be removed before testing the name.
							if strip_tilde (key (component_cursor)) = component then
								component_found := true;
								exit;
							end if;
							next (component_cursor);

						end loop;
						log_indentation_down;
					end query_components;
					
				begin -- generic_name_to_library
					log_indentation_up;
					log ("locating library containing generic component " & to_string (component) & " ...", log_threshold + 2);
					
					-- loop in libraries and exit prematurely once a library with the given component was found
					while lib_cursor /= type_libraries.no_element loop
						log_indentation_up;
						log ("probing " 
							 & et_libraries.to_string (key (lib_cursor)) 
							 & " ...", log_threshold + 3);

						query_element (
							position => lib_cursor,
							process => query_components'access);

						log_indentation_down;
						
						-- Exit BEFORE advancing the lib_cursor because lib_cursor position must be kept fore
						-- return value. See below. component_found MUST NOT be parameter of this loop.
						if component_found then
							exit;
						end if;
						
						next (lib_cursor);

					end loop;
					log_indentation_down;
					
					-- After a successful search return the name of the library where lib_cursor is pointing to.
					-- Otherwise send error messagen and abort.
					if component_found then
						return key (lib_cursor);
					else
						log_indentation_reset;
						log (message_error & "for component "  
							& et_libraries.to_string (reference)
							& " no generic model in any library found !",
							console => true);
						raise constraint_error;
					end if;
					
				end generic_name_to_library;

				procedure insert_component is
				-- Inserts the component in the component list of the module (indicated by module_cursor).
				-- Components may occur multiple times, which implies they are
				-- split into units (EAGLE refers to them as "gates").
				-- Only the first occurence of the component leads to appending it to the component list of the module.
				
				-- The component to be inserted gets assembled from the temporarily variables assigned until now.
				-- Tests if a footprint has been associated with the component.

					function field (
						line		: in et_string_processing.type_fields_of_line;
						position	: in positive) return string renames et_string_processing.get_field_from_line;

					-- KiCad does not provide an exact name of the library where the generic component
					-- model can be found. It only provides the generic name of the model.
					-- The library is determined by the order of the library names in the 
					-- project file. It is the first libraray in this list that contains the model.
					-- The function generic_name_to_library does the job and sets the full_component_library_name
					-- here right away:
					full_component_library_name : type_full_library_name.bounded_string := 
														generic_name_to_library (generic_name_in_lbr, reference);
					
				begin -- insert_component
					log_indentation_up;
					
					-- The component is inserted into the components list of the module according to its appearance.
					-- If the component has already been inserted, it will not be inserted again.
					-- CS: Even if the component is not inserted again, all the operations that form its elements
					-- like power_flag, library_name, ... are executed which is a waste of computing time.
					
					case appearance is
						
						when sch => -- we have a line like "L P3V3 #PWR07"
					
							et_schematic.add_component (
								reference => reference,
								component => (
									appearance		=> sch,

									-- Whether the component is a "power flag" can be reasoned from its reference:
									power_flag		=> to_power_flag (reference),

									library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
									generic_name	=> generic_name_in_lbr,
									
									value 			=> type_component_value.to_bounded_string (content (field_value)),
									commissioned 	=> type_date (et_libraries.content (field_commissioned)),
									updated 		=> type_date (et_libraries.content (field_updated)),
									author 			=> type_person_name.to_bounded_string (content (field_author)),

									-- At this stage we do not know if and how many units there are. So the unit list is empty.
									units 			=> type_units.empty_map),
								log_threshold => log_threshold +1);

						when sch_pcb => -- we have a line like "L 74LS00 U1"

							et_schematic.add_component ( 
								reference => reference,
								component => (
									appearance		=> sch_pcb,

									library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
									generic_name	=> generic_name_in_lbr,
									
									value			=> type_component_value.to_bounded_string (content (field_value)),
									commissioned	=> type_date (content (field_commissioned)),
									updated			=> type_date (content (field_updated)),
									author			=> type_person_name.to_bounded_string (content (field_author)),

									-- properties of a real component (appears in schematic and layout);
									datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),
									partcode		=> type_component_partcode.to_bounded_string (content (field_partcode)),
									purpose			=> type_component_purpose.to_bounded_string (content (field_purpose)),
									bom				=> type_bom'value (content (field_bom)),

									-- the package variant is determined by the package library and package name:
									variant			=> to_package_variant (
															component_library	=> full_component_library_name, -- ../lbr/bel_logic.lib
															generic_name		=> generic_name_in_lbr, -- 7400
															package_library		=> library_name (content (field_package)), -- bel_ic
															package_name		=> package_name (content (field_package)), -- S_SO14
															log_threshold		=> log_threshold + 1),

									position		=> et_pcb.position_placement_default,
									
									-- At this stage we do not know if and how many units there are. So the unit list is empty for the moment.
									units => type_units.empty_map),

								log_threshold => log_threshold +1);

								-- Test if footprint has been associated with the component.
								if content (field_package)'size = 0 then
									log_indentation_reset;
									log (
										text => message_error & "component " & to_string (reference) 
											& " footprint not specified !",
										console => true);
									raise constraint_error;
								end if;

						when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
							null;
							raise constraint_error;
							
					end case;

					log_indentation_down;
					
					exception
						when constraint_error =>
							log_indentation_reset;
							log (
								text => message_error & "component " & et_libraries.to_string (reference)
									& " " & et_coordinates.to_string (position => position),
								console => true);
							raise constraint_error;
					
				end insert_component;
				

				procedure insert_unit is 
				-- Inserts a unit into the unit list of a component. The text fields around a unit are placeholders.
				-- The properties of the placeholder texts are loaded with the properties of the text fields of the units
				-- found in the schematic. The idea behind is to store just basic text properties (type_text_basic) 
				-- for the texts around the unit, but not its content. The content is stored with the component as a kind
				-- of meta-data. See procedure insert_component.
				-- Raises constraint error if unit already in unit list of component.
				begin -- insert_unit
					log_indentation_up;
					
					case appearance is

						when sch =>

							et_schematic.add_unit (
								reference	=> reference,
								unit_name	=> unit_name,
								unit 		=> (
									appearance		=> sch,
									position		=> position,
									orientation		=> orientation,
									mirror			=> mirror,
									name			=> unit_name,
									timestamp		=> timestamp,
									alt_repres		=> alternative_representation,

									-- placeholders:
									-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
									reference		=> (type_text_basic (field_reference)
														with meaning => field_reference.meaning),
									value			=> (type_text_basic (field_value)
														with meaning => field_value.meaning),
									updated			=> (type_text_basic (field_updated)
														with meaning => field_updated.meaning),
									author			=> (type_text_basic (field_author)
														with meaning => field_author.meaning),
									commissioned	=> (type_text_basic (field_commissioned)
														with meaning => field_commissioned.meaning)),
								log_threshold => log_threshold + 1);
												

						when sch_pcb =>

							et_schematic.add_unit (
								reference	=> reference,
								unit_name	=> unit_name,
								unit 		=> (
									appearance		=> sch_pcb,
									position		=> position,
									orientation		=> orientation,
									mirror			=> mirror,
									name			=> unit_name,
									timestamp		=> timestamp,
									alt_repres		=> alternative_representation,

									-- placeholders:
									-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
									reference		=> (type_text_basic (field_reference)
														with meaning => field_reference.meaning),
									value			=> (type_text_basic (field_value)
														with meaning => field_value.meaning),
									packge			=> (type_text_basic (field_package)
														with meaning => field_package.meaning),
									datasheet		=> (type_text_basic (field_datasheet)
														with meaning => field_datasheet.meaning),
									purpose			=> (type_text_basic (field_purpose)
														with meaning => field_purpose.meaning),
									partcode		=> (type_text_basic (field_partcode)
														with meaning => field_partcode.meaning),
									updated			=> (type_text_basic (field_updated)
														with meaning => field_updated.meaning),
									author			=> (type_text_basic (field_author)
														with meaning => field_author.meaning),
									commissioned	=> (type_text_basic (field_commissioned)
														with meaning => field_commissioned.meaning),
									bom				=> (type_text_basic (field_bom)
														with meaning => field_bom.meaning)),
								log_threshold => log_threshold + 1);

						when others => null; -- CS
					end case;

					log_indentation_down;
				end insert_unit;

				procedure verify_unit_name_and_position (line : in type_fields_of_line) is
				-- Checks if the x/y position of the unit matches that provided in given line.
				-- It is about the strange repetition of the unit name and its x/y coordinates in a line like
				-- "2    6000 4000"
					function field (line : in type_fields_of_line; position : in positive) return string renames
						et_string_processing.get_field_from_line;
			
				begin -- verify_unit_name_and_position
					
					if et_libraries.to_string (unit_name) /= field (line,1) then
						raise constraint_error; -- CS: write useful message
					end if;
					
					if distance_x (position) /= mil_to_distance (field (line,2)) then
						log_indentation_reset;
	-- 					log ("position invalid. expected '" & to_string (position.x) 
	-- 						& "' found '" 
	-- 						& field (line,2)
	-- 						& "'");
						raise constraint_error; -- CS: write useful message
					end if;

					if distance_y (position) /= mil_to_distance (field (line,3)) then
						raise constraint_error; -- CS: write useful message
					end if;

				end verify_unit_name_and_position;


				procedure build_unit_orientation_and_mirror_style (line : in type_fields_of_line) is
				-- Builds from a line (see below) the component orientation and mirror style:

					function field (line : in type_fields_of_line; position : in positive) return string renames
						et_string_processing.get_field_from_line;
				
					-- Angles in Kicad are to be interpreted as: 
					-- positive angle -> counter clock wise
					-- negative angle -> clock wise

					-- The order of operations: FIRST rotate THEN mirror
					
					--  1    0    0  -1  -- orientation 0,   mirror normal
					--  0   -1   -1   0  -- orientation 90,  mirror normal
					-- -1    0    0   1  -- orientation 180, mirror normal 
					-- 	0    1    1   0  -- orientation -90, mirror normal  

					-- 	1    0    0   1  -- orientation 0,   mirror --
					--  0   -1    1   0  -- orientation 90,  mirror -- 
					-- -1    0    0  -1  -- orientation 180, mirror -- 
					--  0    1   -1   0  -- orientation -90, mirror -- 

					-- -1    0    0  -1  -- orientation 0,   mirror | 	-- not used
					--  0    1   -1   0  -- orientation 90,  mirror |	-- not used
					--  1    0    0   1  -- orientation 180, mirror |	-- not used
					--  1    0    0   1  -- orientation -90, mirror |	-- not used

					orient_1, orient_2 : type_schematic_unit_orientation;
					mirror_1, mirror_2 : type_schematic_unit_mirror_style;
				
				begin -- CS: provide useful log messages via exception handler

					-- compute unit orientation
					orient_1 := type_schematic_unit_orientation'value (field (line, 1));
					orient_2 := type_schematic_unit_orientation'value (field (line, 2));
					mirror_1 := type_schematic_unit_mirror_style'value (field (line, 3));
					mirror_2 := type_schematic_unit_mirror_style'value (field (line, 4));

					case orient_1 is
						when -1 =>
							if orient_2 = 0 then
								orientation := 180.0;

								-- compute unit mirror style
								if mirror_1 = 0 then
									case mirror_2 is
										when -1 =>
											mirror := x_axis;
										when  1 =>
											mirror := none;
										when others =>
											-- invalid mirror style
											raise constraint_error;
									end case;
								else
									-- invalid mirror style
									raise constraint_error;
								end if;
								
							else
								-- invalid orientation
								raise constraint_error;
							end if;
								
						when  0 =>
							case orient_2 is
								when -1 => 
									orientation :=  90.0;
									
									-- compute unit mirror style
									case mirror_1 is
										when -1 =>
											if mirror_2 = 0 then
												mirror := none;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;

										when  0 =>
											-- invalid mirror style
											raise constraint_error;

										when  1 =>
											if mirror_2 = 0 then
												mirror := x_axis;
											else
												-- invaid mirror style
												raise constraint_error;
											end if;
									end case;

								when  1 =>
									orientation := -90.0;

									-- compute unit mirror style
									case mirror_1 is
										when -1 =>
											if mirror_2 = 0 then
												mirror := x_axis;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;

										when  0 =>
											-- invaid mirror style
											raise constraint_error;

										when  1 =>
											if mirror_2 = 0 then
												mirror := none;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;
									end case;

								when others => 
									-- invalid orientation
									raise constraint_error;
							end case;

						when  1 =>
							if orient_2 = 0 then
								orientation := 0.0;

								-- compute unit mirror style
								if mirror_1 = 0 then
									case mirror_2 is
										when -1 =>
											mirror := none;
										when  1 =>
											mirror := x_axis;
										when others =>
											-- invalid mirror style
											raise constraint_error;
									end case;
								else
									-- invalid mirror style
									raise constraint_error;
								end if;
								
							else
								-- invalid orientation
								raise constraint_error;
							end if;
					end case;

				end build_unit_orientation_and_mirror_style;

				
				use type_lines;
			
			begin -- make_component (schematic)
				
				line_cursor := type_lines.first (lines);
				while line_cursor /= type_lines.no_element loop

					log ("component line: " & to_string (et_kicad.line), log_threshold + 4);

					-- Read component name and annotation from a line like "L NetChanger N1". 
					-- From this entry we reason the component appearance. 
					-- The appearance is important for contextual validation of the fields (like field_partcode, field_bom, ...).
					-- It is also required for validation of the reference (like R12 or C4).
					if field (et_kicad.line,1) = schematic_component_identifier_name then -- "L"
						
						generic_name_in_lbr := type_component_generic_name.to_bounded_string (field (et_kicad.line,2)); -- "SN74LS00"
						log ("generic name " & to_string (generic_name_in_lbr), log_threshold + 3);
						
						check_generic_name_characters (
							name => generic_name_in_lbr, -- "SN74LS00"
							-- NOTE: We do not allow tilde characters here. they occur ONLY in the library:
							characters => et_libraries.component_generic_name_characters); 

						appearance := to_appearance (line => et_kicad.line, schematic => true);
						log (to_string (appearance), log_threshold + 3);

						-- Depending on the appearance of the component the reference is built and checked.
						case appearance is
						
							when et_libraries.sch => 
								-- We have a line like "L P3V3 #PWR07".
								-- Build a reference type from the given reference string.
								-- Afterward we validate the prefix of the reference. It must be
								-- a power symbol or a power flag (#PWR or #FLG).
								reference := to_component_reference (
									text_in => field (et_kicad.line,3),
									allow_special_character_in_prefix => true); 

								log ("reference " & to_string (reference), log_threshold + 3);
								validate_prefix (reference);

							when et_libraries.sch_pcb =>
								-- we have a line like "L 74LS00 IC13"
								-- -- Build a reference type from the given reference string.
								-- Afterward we validate the prefix of the reference. 
								-- It is about a REAL component. Its prefix must be one 
								-- of those defined in the configuration file (see et_configuration).
								reference := to_component_reference ( -- character check included
									text_in => field (et_kicad.line,3),
									allow_special_character_in_prefix => false);

								log ("reference " & to_string (reference), log_threshold + 3);
								
								et_configuration.validate_prefix (reference);

							when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
								null;
								raise constraint_error;
								
						end case;
									
						-- CS: check proper annotation

					-- read line like "U 2 1 4543D4D3F" 
					-- U is the line indicator, 2 is the unit id, 1 is the demorgan flag, last field is the timestamp
					elsif field (et_kicad.line,1) = schematic_component_identifier_unit then -- "U"

						-- KiCad uses positive numbers to identifiy units. But in general a unit name can
						-- be a string as well. Therefore we handle the unit id as string.
						unit_name := type_unit_name.to_bounded_string ( -- CS: check_unit_name_characters
							field (et_kicad.line,2)); -- the unit id

						-- Read DeMorgan flag:
						alternative_representation := to_alternative_representation (line => et_kicad.line, schematic => true);

						-- Read and check the timestamp:
						timestamp := type_timestamp (field (et_kicad.line,4));
						et_string_processing.check_timestamp (timestamp);

					-- Read unit coordinates from a line like "P 3200 4500".
					elsif field (et_kicad.line,1) = schematic_component_identifier_coord then -- "P"
					
						set_x (position, mil_to_distance (field (et_kicad.line,2))); -- "3200"
						set_y (position, mil_to_distance (field (et_kicad.line,3))); -- "4500"

						-- The unit coordinates is more than just x/y :
						set_path (position, path_to_submodule);
						set_sheet (position, sheet_number_current);

					-- Skip unit path entry in lines like "AR Path="/59EF082F" Ref="N23"  Part="1"
					elsif field (et_kicad.line,1) = schematic_component_identifier_path then -- "AR"
						-- CS: meaning unclear
						log (message_warning & affected_line (et_kicad.line) 
							& "ignoring line '" & to_string (et_kicad.line) & "' ! Meaning unclear !");

					-- read unit fields 0..2 from lines like:
					-- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
					--			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
					--			"F 2 "bel_netchanger:0.2MM" H 2600 2100 60  0001 C CNN"

					-- Set "field found flags" accordingly.
					-- Do some basic checks on the fields.
					elsif field (et_kicad.line,1) = component_field_identifier then -- "F"

						--log ("unit field A: " & to_string (et_kicad.line));
						
						case type_component_field_id'value (field (et_kicad.line,2)) is
							
							when component_field_reference =>
								field_reference_found := true;
								field_reference := to_field;
								check_reference_characters (
									reference => content (field_reference),
									characters => et_kicad.component_reference_characters);
								-- NOTE: This is a redundant field. Its content must match the reference (see above).
								-- This test is performed in procedure check_text_fields.
								
							when component_field_value =>
								field_value_found := true;
								field_value := to_field;
								check_value_length (content (field_value));
								check_value_characters (
									value => type_component_value.to_bounded_string (content (field_value)),
									characters => component_value_characters);
								
							when component_field_package =>
								field_package_found := true;
								field_package := to_field;
								check_package_name_length (content (field_package));
								check_package_name_characters (
									packge => type_component_package_name.to_bounded_string (content (field_package)),
									characters => et_kicad.component_package_name_characters);
								
							when component_field_datasheet =>
								field_datasheet_found := true;
								field_datasheet := to_field;
								check_datasheet_length (content (field_datasheet));
								check_datasheet_characters (
									datasheet => type_component_datasheet.to_bounded_string (content (field_datasheet)));
								
							when component_field_purpose =>
								field_purpose_found	:= true;
								field_purpose := to_field;
								check_purpose_length (content (field_purpose));
								check_purpose_characters (
									purpose => type_component_purpose.to_bounded_string (content (field_purpose)),
									characters => component_initial_purpose_characters);
								
							when component_field_partcode =>
								field_partcode_found := true;
								field_partcode := to_field;
								check_partcode_length (content (field_partcode));
								check_partcode_characters (
									partcode => type_component_partcode.to_bounded_string (content (field_partcode)),
									characters => component_initial_field_characters);
								
							when component_field_commissioned =>
								field_commissioned_found := true;
								field_commissioned := to_field;
								check_date_length (content (field_commissioned));
								check_date_characters (
									date => type_component_date (content (field_commissioned)));
								
							when component_field_updated =>
								field_updated_found	:= true;
								field_updated := to_field;
								check_date_length (content (field_updated));
								check_date_characters (
									date => type_component_date (content (field_updated)));
								
							when component_field_author =>
								field_author_found := true;
								field_author := to_field;
								check_author_length (content (field_author));
								check_author_characters (
									author => type_component_author.to_bounded_string (content (field_author)));
								
							when component_field_bom =>
								field_bom_found := true;
								field_bom := to_field;
								-- NOTE: length check already included in check_bom_characters
								check_bom_characters (content (field_bom));

							when others => invalid_field (et_kicad.line); -- other fields are not accepted and cause an error
						end case;

						--log ("unit field B: " & to_string (et_kicad.line));
						
					else
						-- What is left is a strange repetition of the unit name and its x/y coordinates in a line like
						-- "2    6000 4000"
						-- followed by the unit mirror style and the unit orientation in a line like
						-- "1    0    0    -1"

						case field_count (et_kicad.line) is
							when 3 => -- we have the unit name and its x/y position.
								-- We verify if unit name and position match the values read earlier:
								verify_unit_name_and_position (et_kicad.line);
							
							when 4 => null; -- we have the unit mirror style and orientation
								build_unit_orientation_and_mirror_style (et_kicad.line);
							
							when others => 
								raise constraint_error; -- CS: write useful message
						end case;

					end if;


					next (line_cursor);
				end loop;

				-- Check if all required text fields have been found.
				-- Check content of text fields for syntax and plausibility.
				check_text_fields (log_threshold + 1);
				
				-- Insert component in component list of module. If a component is split
				-- in units, only the first occurence of it leads to inserting the component.
				-- Nevertheless there are some checks on the unit (see insert_component).
				insert_component;

				-- We update the component with the collected unit information.
				insert_unit;

				exception
					when others => 
						error_in_schematic_file (et_kicad.line);
						raise;
				
			end make_component;

			function no_connection_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a no-connection-flag "NoConn ~ 5000 3900"
				result : boolean := false;
			begin
				-- CS test field count
				if field (line,1) = schematic_keyword_no_connection then
					if field (line,2) = schematic_tilde then
						result := true;
					end if;
				end if;
				return result;
			end no_connection_header;
			
			procedure make_no_connection (line : in type_fields_of_line) is
			-- Builds a no-connect flag and stores it a wild list of no-connection-flags
			-- A line that specifies such a flag loops like "NoConn ~ 5000 3900"
				no_connection_flag : et_schematic.type_no_connection_flag;

				use type_rig;
			
				procedure append_no_connect_flag (
					module_name : in type_submodule_name.bounded_string;
					module : in out type_module) is
					use type_no_connection_flags;
				begin
					append (
						container => module.no_connections,
						new_item => no_connection_flag);
				end append_no_connect_flag;				
			
			begin
				set_path (no_connection_flag.coordinates, path_to_submodule);
				set_sheet (no_connection_flag.coordinates, sheet_number_current);
				set_x (no_connection_flag.coordinates, mil_to_distance (field (line,3)));
				set_y (no_connection_flag.coordinates, mil_to_distance (field (line,4)));

				-- for the log
				if log_level >= log_threshold + 1 then
					log_indentation_up;
					log ("no-connection-flag at " 
						& to_string (no_connection_flag => no_connection_flag, scope => xy));
					log_indentation_down;
				end if;

				-- append the no-connect-flag to the list of no_connections of the current module
				update_element (
					container => rig,
					position => module_cursor,
					process => append_no_connect_flag'access);
				
			end make_no_connection;

			
		begin -- read_schematic
			log_indentation_reset;
			log_indentation_up;
		
			if exists (to_string (current_schematic)) then
				log ("reading schematic file " & to_string (current_schematic) & " ...",
					 log_threshold + 1,
					 console => true);

				-- log module path as recorded by parent unit
				log_indentation_up;
				log ("path " & to_string (path_to_submodule), log_threshold + 1);
				
				open (file => schematic_handle, mode => in_file, name => to_string (current_schematic));
				set_input (schematic_handle);

				-- read schematic file line per line
				while not end_of_file loop

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
								line => get_line,
								number => ada.text_io.line (current_input),
								comment_mark => "", -- there are no comment marks in the schematic file
								delimiter_wrap => true, -- there are fields wrapped in delimiters
								ifs => latin_1.space); -- fields are separated by space
					-- CS: If read_line exits with an exception, the exception handler of read_schematic
					-- outputs the line BEFORE the faulty line. Thus misleading the operator.
					
					case field_count (line) is
						when 0 => null; -- we skip empty lines
						when others =>

							-- At a certain log level we report the whole line as it is:
 							log (to_string (line), log_threshold + 3);

							-- The first line should be the headline with the schematic version:
							-- READ SCHEMATIC HEADLINE:

							-- EESchema Schematic File Version 2
							
							if not schematic_version_valid then
								check_header (line); 
								-- sets schematic_version_valid true.
								-- aborts program if version not supported
							end if;

							-- READ SHEET HEADER:

							--	EESchema Schematic File Version 2
							--	LIBS:nucleo_core-rescue
							--	LIBS:power
							--	LIBS:bel_connectors_and_jumpers
							--	LIBS:bel_primitives
							--	LIBS:bel_stm32
							--	LIBS:nucleo_core-cache
							--	EELAYER 25 0
							--	EELAYER END

							if not sheet_header_entered then
								if get_field_from_line (field  (line,1), 1, latin_1.colon) = schematic_library then
									sheet_header_entered := true;
									add (line);
								end if;
							else -- we are inside the sheet header and wait for the footer
								if field (line,1) = schematic_eelayer and field (line,2) = schematic_eelayer_end then
									sheet_header_entered := false;
									add (line);
									make_sheet_header (lines);
									clear (lines); -- clean up line collector
								else
									add (line);
								end if;
							end if;
							
							-- READ DESCRIPTION:

							-- $Descr A4 11693 8268
							-- encoding utf-8
							-- Sheet 5 8
							-- Title ""
							-- Date ""
							-- Rev ""
							-- Comp ""
							-- Comment1 ""
							-- Comment2 ""
							-- Comment3 ""
							-- Comment4 ""
							-- $EndDescr
							
							if not description_entered then
								if field (line,1) = schematic_description_header then -- $Descr A4 11693 8268
									description_entered := true; -- we are entering the sheet description

									add (line);
								end if;
							else -- we are inside the description
								if field (line,1) = schematic_description_footer then -- $EndDescr
									description_entered := false; -- we are leaving the description
									description_processed := true;

									make_drawing_frame (lines, log_threshold + 1);
									clear (lines); -- clean up line collector
								else
									add (line);
								end if;
							end if;

							-- Read hierarchical GUI sheets (if there has been a total sheet count greater 1 detected earlier).
							-- A hierachical GUI sheet displays a hierarchical sheet as a black box with its ports.
							-- It serves as link between a hierachical net and the parent module.
							-- Rightly said this is the black box representation of a submodule. 
							-- So in the following we refer to them as "submodule".
							-- A submodule (sheet) section example:
							
							-- $Sheet
							-- S 4050 5750 1050 650 
							-- U 58A73B5D
							-- F0 "Sheet58A73B5C" 58
							-- F1 "morpho_test.sch" 58
							-- F2 "SENSOR_GND" U R 2250 3100 60 
							-- F3 "SENSOR_VCC" I L 1350 3250 60 
							-- $EndSheet

							if sheet_count_total > 1 then -- read hierarchical GUI sheets
								if not sheet_description_entered then
									if field (line,1) = schematic_sheet_header then -- $Sheet
										sheet_description_entered := true;
									end if;
								else -- we are inside a sheet description
									if field (line,1) = schematic_sheet_footer then -- $EndSheet
										sheet_description_entered := false; -- we are leaving the sheet description

										make_gui_sheet (lines, log_threshold + 1);
										clear (lines);
									else
										--log (to_string (line));
										add (line);
									end if;
								end if;
							end if;

							-- Further parts of the file can be read IF the description has been processed before (see above)
							if description_processed then
								
								-- READ NET SEGMENTS

								-- Wire Wire Line
								-- 2250 3100 2400 3100 

								if not net_segment_entered then
									if net_segment_header (line) then
										net_segment_entered := true;
									end if;
								else
									net_segment_entered := false; -- we are leaving a net segment
									add (line);
									make_net_segment (lines, log_threshold + 1);
									clear (lines);
								end if;

								-- READ NET JUNCTIONS 
								
								-- Connection ~ 4650 4600

								if junction_header (line) then
									make_junction (line, log_threshold + 1);
								end if;
									
								-- READ SIMPLE NET LABELS (they do not have a tag, but just a text) 

								-- Text Label 2350 3250 0 60 ~ 0
								-- TOP_VCC

								if not simple_label_entered then
									if simple_label_header (line) then
										simple_label_entered := true;
										add (line);
									end if;
								else
									simple_label_entered := false; -- we are leaving a simple label
									add (line);
									make_simple_label (lines, log_threshold + 1);
									clear (lines);
								end if;
								
								-- READ TAG NET-LABELS (global or hierarchical)

								-- Text GLabel 4700 3200 1    60   UnSpc ~ 0
								-- DRV_1

								if not tag_label_entered then
									if tag_label_header (line) then
										tag_label_entered := true;
										add (line);
									end if;
								else
									tag_label_entered := false; -- we are leaving a tag label
									add (line);
									make_tag_label (lines, log_threshold + 1);
									clear (lines);
								end if;

								-- READ NOTES 

								-- "Text Notes 3400 2800 0 60 Italic 12"
								-- "ERC32 Test Board"
								
								if not note_entered then
									if text_note_header (line) then
										note_entered := true; -- we are entering a note
										add (line);
									end if;
								else 
									note_entered := false; -- we are leaving a note
									add (line);
									make_text_note (lines, log_threshold + 1);
									clear (lines);
								end if;
								
								-- READ COMPONENTS
								-- Once a component header ($Comp) found, set component_entered flag. This indicates we are inside a component section.
								-- Inside the component section, we process its content until the component footer ($EndComp) is found.
								-- Some entries of the component section are relevant for the whole component. Some entries are unit specific.
								-- The component section looks like this example:
								
								-- $Comp
								-- L 74LS00 U1		-- component specific
								-- U 4 1 5965E676	-- unit specific
								-- P 4100 4000		-- unit specific
								-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
								-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
								-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
								-- F 3 "" H 4100 4000 50  0001 C CNN
								-- 	4    4100 4000		-- CS: same as x/y pos ?

								--  1    0    0  -1  -- orientation 0,   mirror normal
								--  0   -1   -1   0  -- orientation 90,  mirror normal
								-- -1    0    0   1  -- orientation 180, mirror normal 
								-- 	0    1    1   0  -- orientation -90, mirror normal  

								-- 	1    0    0   1  -- orientation 0,   mirror --
								--  0   -1    1   0  -- orientation 90,  mirror -- 
								-- -1    0    0  -1  -- orientation 180, mirror -- 
								--  0    1   -1   0  -- orientation -90, mirror -- 

								-- -1    0    0  -1  -- orientation 0,   mirror |
								--  0    1   -1   0  -- orientation 90,  mirror |
								--  1    0    0   1  -- orientation 180, mirror |
								--  1    0    0   1  -- orientation -90, mirror |
								-- $EndComp

								if not component_entered then
									if component_header (line) then
										component_entered := true;
									end if;
								else -- we are inside the component and wait for the component footer ($EndComp)
									if component_footer (line) then
										component_entered := false; -- we are leaving the component

										make_component (lines);
										clear (lines);
									else -- read lines of unit/component
										add (line);
									end if;
								
								end if;

								-- READ NO-CONNECT-FLAGS

								-- NoConn ~ 5000 3900
								
								if no_connection_header (line) then
									make_no_connection (line);
								end if;
							end if;

					end case;

				end loop;

				close (schematic_handle);
				log_indentation_down;
				log ("reading complete. closing schematic file " & to_string (current_schematic) & " ...", log_threshold);

				-- From the wild list of net segments, assemble net segments to anonymous strands.
				-- A strand is: all net segments connected with each other by their start or end points.
				build_anonymous_strands;
	
				-- All anonymous strands must be given a name. The name is enforced by the a net label.
				-- (The fact that power-put ports enforce a name also, is cared for later on netlist generation.)
				-- The first label found on the strand sets the strand name and scope. 
				-- Other labels on the strand are checked for their name only. 
				-- If the name differs from the net name set earlier, a warning is output. 
				-- Strands without label remain anonymous. Their name is assigned by using the notation "N$".
				-- The strands are finally appended to the strands of the current module (see spec. of type_module.strands).
				associate_net_labels_with_anonymous_strands;

			else
				log_indentation_reset;
				log (message_error & "schematic file '" & to_string (current_schematic) & "' not found !",
					console => true);
				raise constraint_error;
			end if;

			return list_of_submodules;

			exception
				when others =>
					error_in_schematic_file (line);
					et_import.close_report;
-- 					put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;					

		end read_schematic;

		module_name : type_submodule_name.bounded_string; -- the name of the module to be created
		module_inserted : boolean := false; -- goes true if module already created. should never happen

	begin -- import_design

		-- change to given project directory
		log (
			text => "changing to project directory " & (type_project_name.to_string (project) & " ..."),
			level => log_threshold
			);
		set_directory (type_project_name.to_string (project));
		
		case et_import.cad_format is
			when et_import.kicad_v4 =>

				-- Kicad uses Y axis positive downwards style
				Y_axis_positive := downwards;
				
				-- derive top level schematic file name from project name
				top_level_schematic	:= read_project_file;
				
				-- The top level schematic file dictates the module name. 
				-- If parameter first_instance is true, the name of the first
				-- instance must be appended to the module_name.
				if first_instance then 
					-- Append instance to module name
					module_name := append_instance (
										submodule =>
											type_submodule_name.to_bounded_string (
												base_name (to_string (top_level_schematic))),
										instance => type_submodule_instance'first);
				else
					-- default mode: regular design import. set module name as top_level_schematic
					module_name := type_submodule_name.to_bounded_string (
											base_name (to_string (top_level_schematic)));
				end if;

				-- create the module:
				type_rig.insert (
					container	=> rig,
					key			=> module_name,
					new_item 	=> (
						generic_name	=> type_submodule_name.to_bounded_string (
											base_name (to_string (top_level_schematic))),
						instance		=> type_submodule_instance'first,
						
						libraries		=> tmp_project_libraries, -- set project libraries (collected via project file)
						strands			=> type_strands.empty_list,
						junctions		=> type_junctions.empty_list,
						nets			=> type_nets.empty_map,
						components		=> type_components.empty_map,
						no_connections	=> type_no_connection_flags.empty_list,
						portlists		=> type_portlists.empty_map,
						netlist			=> type_netlist.empty_map,
						submodules		=> type_gui_submodules.empty_map,
						frames			=> type_frames.empty_list,
						title_blocks	=> type_title_blocks.empty_list,
						notes			=> type_texts.empty_list,
						sheet_headers	=> type_sheet_headers.empty_map),

					position	=> module_cursor,
					inserted	=> module_inserted);

				if not module_inserted then -- CS should never happen
					log_indentation_reset;
					log (message_error & "module " & to_string (module_name) & " already in rig !");
					raise constraint_error;
				end if;
				
				read_components_libraries (log_threshold); -- as stored in element "libraries" of the current module
				current_schematic := top_level_schematic;
				check_submodule_name_characters (to_submodule_name (current_schematic));
				
                -- The top level schematic file is the first entry in the module path.
				-- The top level schematic file is the root in the module path.
				-- Starting from the top level module, we read its schematic file. The result can be a list 
				-- of submodules which means that the design is hierarchic.
				-- NOTE: Kicad refers to submodules as "sheets" !
				
				-- The function read_schematic requires the name of the current submodule,
				-- It returns a list of submodules.
				list_of_submodules := read_schematic (current_schematic, log_threshold);

				log("DESIGN STRUCTURE ");
				log_indentation_up;
				
				-- If read_schematic returns an empty list of submodules, we are dealing with a flat design. Otherwise
				-- the design is hierarchic (because the submodule list is longer than zero).
				if type_submodule_names.is_empty (list_of_submodules.list) then -- flat design
					log ("FLAT");
				else -- hierarchic design
					-- In the following we dive into the submodules. Each time before a deeper level is entered,
					-- the list of submodules (of the current level) is saved on a LIFO stack.
					-- The top level schematic is at level 0. The level decreases (negative) each time a deeper
					-- level is entered.
					log ("HIERARCHIC");
					
					stack_of_sheet_lists.init; -- stack init

					log_indentation_up;
					
					-- output the number of submodules (sheets) found at level 0:
					log ("number of hierarchic sheets" & natural'image (
						natural (type_submodule_names.length (list_of_submodules.list)))); -- CS: use count_type

					-- Initially set submodule pointer at first submodule of list:
					list_of_submodules.id := 1;
                    
					loop
						-- fetch name of submodule (where id is pointing to)
						current_schematic := type_schematic_file_name.to_bounded_string (
							et_coordinates.type_submodule_name.to_string (
								type_submodule_names.element (
									container => list_of_submodules.list,
									index => list_of_submodules.id)));

						check_submodule_name_characters (to_submodule_name (current_schematic));
						
						-- backup list_of_submodules OF THIS LEVEL on stack (including the current submodule id)
						push (list_of_submodules);
						
						log ("DESCENDING TO HIERARCHY LEVEL -" & trim (natural'image (depth),left));
						log (row_separator_single);

						append_name_of_parent_module_to_path (type_submodule_name.to_bounded_string (base_name (to_string (current_schematic)))); -- EXP
						-- Read schematic file as indicated by list_of_submodules.id. 
						-- Read_schematic receives the name of the schematic file to be read.
						list_of_submodules := read_schematic (current_schematic, log_threshold);

						-- If the schematic file contains submodules (hierarchic sheets), set list_of_submodules.id to the first 
						-- submodule of them. Otherwise restore submodule list of parent module and advance therein to next submodule.
						if type_submodule_names.is_empty (list_of_submodules.list) then -- flat submodule (no hierarchic sheets)

							list_of_submodules := pop;
							delete_last_module_name_from_path; -- EXP
                            list_of_submodules.id := list_of_submodules.id + 1;
                            
							log ("NO SUBMODULES HERE. ASCENDING TO HIERARCHY LEVEL -" & trim (natural'image (depth),left));
							log (row_separator_single);

						else
							-- set cursor at first submodule of list and append name of parent module to path_to_submodule
                            list_of_submodules.id := 1;
                            --append_name_of_parent_module_to_path (list_of_submodules.parent_module); -- EXP
						end if;

						-- Once the last submodule of the list has been processed, restore list of the overlying level and advance to next module.
						-- Exit after last submodule in level 0 has been processed.
						if list_of_submodules.id > positive (type_submodule_names.length (list_of_submodules.list)) then
							if depth = 0 then 
								log ("LAST SUBMODULE PROCESSED.");
								exit; 
							end if;
							list_of_submodules := pop; -- restore overlying list
							delete_last_module_name_from_path; -- update path_to_submodule -- EXP
                            list_of_submodules.id := list_of_submodules.id + 1;
--                             delete_last_module_name_from_path; -- update path_to_submodule -- EXP
							log ("LAST SUBMODULE PROCESSED. ASCENDING TO HIERARCHY LEVEL: -" & trim(natural'image(depth),left));
							log (row_separator_single);
						end if;
						
					end loop;

					--log_indentation_down;

					-- Checks scope of strands across the current module (indicated by module_cursor).
					-- NOTE: module_cursor points to the current module.
					--check_strands; -- CS: currently not used and does not belong here
					
				end if;

				
				log_indentation_down;

				-- Update strand names according to power out ports connected with them:
				et_schematic.update_strand_names (log_threshold + 1); -- includes portlist generation

				-- write strands report
				et_schematic.write_strands (log_threshold + 1);
				
				-- Merge the strands which are still independed of each other. 
				-- For example a strand named "VCC3V3" exists on submodule A on sheet 2. 
				-- Another strand "VCC3V3" exists on submodule C on sheet 1. They do not "know" each other
				-- and must be merged into a single net.
				et_schematic.link_strands (log_threshold + 1);

				-- Append hierarchic strands to global or local nets.
				-- IMPORTANT: Hierarchic nets are nothing more than extensions of
				-- local or global nets !
				et_schematic.process_hierarchic_nets (log_threshold + 1);

				-- write net report
				et_schematic.write_nets (log_threshold + 1);

			when others =>
				null; -- CS: add import of other CAD formats here

				
		end case;

		
		
-- 		exception
-- 			-- CS: log exception message
-- 			when event:
-- 				constraint_error =>
-- 					log_indentation_reset;
-- 					log (message_error & "in schematic file '" 
-- 						& to_string (current_schematic) & "' " 
-- 						console => true);
-- 						et_import.close_report;
-- 						put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
-- 					raise;

		
	end import_design;

end et_kicad;

-- Soli Deo Gloria
