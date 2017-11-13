------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_netlist;

with et_geometry;

with et_general;
with et_string_processing;		use et_string_processing;

package body et_kicad is


	procedure invalid_field (line : in type_fields_of_line) is
	-- CS: display field meaning ?
		-- 	meaning : in et_general.type_text_meaning) return string is
	begin
		log_indentation_reset;
		log (text => message_error & affected_line (line) & "invalid field !",
			console => true);

		log (text => to_string (line),
			console => true);

		-- CS: refine output.
		raise constraint_error;
	end invalid_field;

	
	function to_text_meaning (
	-- Extracts from a scheamtic field like "F 0 "#PWR01" H 2000 3050 50  0001 C CNN" its meaning.
	-- Extracts from a component field like "F0 "IC" 0 50 50 H V C CNN" its meaning.
	-- Since the fields start different in libaray and schematic we also need a flag that tells
	-- the function whether we are dealing with scheamtic or library fields.
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
				if field(line,1) = component_field_identifier then

					-- Then we test the field id.
					-- The field id must be mapped to the actual field meaning:
					case type_component_field_id'value(field(line,2)) is -- "0..2"
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_footprint		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_function		=> meaning := et_libraries.purpose;
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

				-- In a library the meaning of a text field is identified by "F0 ...".
				
				-- So the first thing to do is test if the letter F at the begin of the line:
				if strip_id(field(line,1)) = component_field_identifier then
				
					case type_component_field_id'value ( strip_f (field(line,1) ) ) is
						when component_field_reference		=> meaning := et_libraries.reference;
						when component_field_value			=> meaning := et_libraries.value;
						when component_field_footprint		=> meaning := et_libraries.packge;
						when component_field_datasheet		=> meaning := et_libraries.datasheet;
						when component_field_function		=> meaning := et_libraries.purpose;
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
		-- CS: display field meaning ?
			-- 	meaning : in et_general.type_text_meaning) return string is
		begin
			log_indentation_reset;
			log (text => message_error & et_string_processing.affected_line(line) & "invalid visibility flag !",
				console => true);
		
				-- CS: refine output.
			raise constraint_error;
		end invalid_appearance;	

		use et_libraries;

	begin -- to_appearance
		case schematic is

			when true =>
				--put(3 * latin_1.space & keyword_appears);
				
				-- If it is about a schematic component we just test if the first
				-- character of the 3ed subfield is a hash sign.
				if field(line,3)(field(line,3)'first) = schematic_component_power_symbol_prefix then
					comp_app := sch;
				else
					comp_app := sch_pcb;
				end if;
				
			when false =>
				--put(4 * latin_1.space & keyword_appears);
				
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value(field(line,10));

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
	
		function field ( line : in type_fields_of_line; pos : in positive) return string 
			renames et_string_processing.get_field_from_line;
		
		rep_in : type_alternative_representation;
		rep_out : et_schematic.type_alternative_representation;
	begin
		rep_in := type_alternative_representation'value (field (line,3));

		case rep_in is
			when alternative_representation_yes =>
				rep_out := yes;

				-- CS: currently we do not support alternative representations
				log_indentation_reset;
				log (text => message_error & "alternative representation (DeMorgan) not supported currently !",
					console => true);
				
			when alternative_representation_no =>
				rep_out := no;
		end case;
		
		return rep_out;
		
		-- CS exception handler
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

	function to_power_flag (reference : in et_libraries.type_component_reference) return boolean is
	-- If the given component reference is one that belongs to a "power flag" returns true.
		use et_libraries.type_component_prefix;
	begin
		--log (et_schematic.to_string (reference));
		if et_libraries.prefix (reference) = power_flag_prefix then
			--log ("power flag on");
			return true;
		else
			--log ("power flag off");
			return false;
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

		
		procedure read_library is
			line				: type_fields_of_line; -- the line being processed

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
			-- These properties apply for the whole component (means for all its units): -- CS: move them to specs ?
			tmp_component_name		: type_component_name.bounded_string; -- 74LS00
			tmp_prefix				: type_component_prefix.bounded_string; -- IC
			tmp_appearance			: type_component_appearance;

			tmp_port_name_visible	: type_port_visible;
			tmp_pin_name_visible	: type_pin_visible;
			tmp_port_name_offset	: type_distance;
			
			tmp_units_total		: type_units_total; -- see spec for range			
			tmp_unit_id			: type_unit_id; -- assumes 0 if all units are affected, -- see spec			
			tmp_unit_swap_level	: type_unit_swap_level := unit_swap_level_default;
			tmp_unit_add_level	: type_unit_add_level := type_unit_add_level'first;
			tmp_unit_global		: boolean := false; -- specifies if a unit harbors component wide pins (such as power supply)
			
			tmp_reference		: type_text (meaning => reference);
			tmp_value			: type_text (meaning => value);
			tmp_commissioned	: type_text (meaning => commissioned);
			tmp_updated			: type_text (meaning => updated);
			tmp_author			: type_text (meaning => author);
			tmp_package			: type_text (meaning => packge);
			tmp_datasheet		: type_text (meaning => datasheet);
			tmp_purpose			: type_text (meaning => purpose);
			tmp_partcode		: type_text (meaning => partcode);
			tmp_bom				: type_text (meaning => bom);

			-- temporarily used variables to store draw elements (polylines, arcs, pins, ...) 
			-- before they are added to a unit.
			tmp_draw_polyline	: type_polyline;
			tmp_draw_rectangle	: type_rectangle;
			tmp_draw_arc		: type_arc;
			tmp_draw_circle 	: type_circle;
			tmp_draw_text		: type_symbol_text;
			tmp_draw_port		: type_port;
-- 			tmp_draw_port_name	: type_port_name.bounded_string; -- CS: maybe not used anymore
			
			procedure init_temp_variables is
			begin
				-- CS: init other variables
				extra_unit_available := false;
				tmp_unit_add_level := type_unit_add_level'first;
				tmp_unit_global := false;
			end init_temp_variables;

			procedure check_text_fields is
			begin
				null; -- CS: do a value check if value provided
			end check_text_fields;
			
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
			-- Converts the kicad "show pin number" flag to the et type_pin_visible.
			-- Used when reading component libraries.		
				return type_pin_visible is

				v_in	: type_show_pin_number;
				v_out	: type_pin_visible;
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
			-- Converts the kicad "show pin name" flag to the et type_port_visible.
			-- Used when reading component libraries.		
				return type_port_visible is

				v_in	: type_show_pin_name;
				v_out	: type_port_visible;
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
			-- Returns from the given fields of a port as a type_port.
				port	: type_port;

				function field (line : in type_fields_of_line; position : in positive) return string renames
					et_string_processing.get_field_from_line;

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
							d_out := PULL_LOW;

						when library_pin_electrical_type_open_emitter => 
							d_out := PULL_HIGH;

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
				port.name := type_port_name.to_bounded_string (field (line,2));
				
				-- compose pin name
				port.pin			:= type_pin_name.to_bounded_string (field (line,3));

				-- compose position
				set_x (port.coordinates, mil_to_distance (mil => field (line,4), warn_on_negative => false));
				set_y (port.coordinates, mil_to_distance (mil => field (line,5), warn_on_negative => false));
				mirror (point => port.coordinates, axis => x);

				-- compose length
				port.length			:= mil_to_distance (mil => field (line,6), warn_on_negative => false);

				-- compose orientation
				-- CS: port.orientation	:= type_library_pin_orientation

				-- port and pin name text size
				port.pin_name_size	:= mil_to_distance (mil => field (line,8), warn_on_negative => false);
				port.port_name_size	:= type_text_size'value (field (line,9));

				-- direction
				port.direction		:= to_direction (field (line,12));

				-- port style (optional, to be composed if field #13 present)
				if field_count (line) = 13 then
					port.style		:= to_style (field (line,13));
				end if;

				-- visibility port and pin names
				port.port_name_visible	:= tmp_port_name_visible;
				port.pin_name_visible	:= tmp_pin_name_visible;

				-- port name offset
				port.port_name_offset	:= tmp_port_name_offset;

				--log (text => et_coordinates.to_string (point => port.coordinates), level => 1);

				-- CS: log other port properties

				log_indentation_down;
				return port;

				-- CS: exception handler
			end to_port;

					
			function read_field (meaning : in type_text_meaning) return type_text is
			-- Reads general text field properties from subfields 3..9 and returns a type_text with 
			-- the meaning as given in parameter "meaning".
			-- Checks basic properties of text fields (text size, aligment, ...)
				use et_coordinates;
				text : type_text(meaning);

			begin
				-- field #:
				-- 3/4 : x/y coordinates
				-- 5 : size
				-- 6 : orientation (H/V)
				-- 7 : visible/invisible (V/I)
				-- 8 : aligment horizontal (R,C,L)
				-- 9 : aligment vertical (TNN, CNN, BNN) / font normal, italic, bold, bold_italic (TBI, TBN)

				text.content		:= type_text_content.to_bounded_string (strip_quotes (get_field_from_line (line,2)));
				-- CS: check content vs. meaning
				
				set_x (text.position, mil_to_distance (mil => get_field_from_line (line,3), warn_on_negative => false));
				set_y (text.position, mil_to_distance (mil => get_field_from_line (line,4), warn_on_negative => false));
				text.size 			:= mil_to_distance (mil => get_field_from_line (line,5), warn_on_negative => false);

				-- check text size of fields
				if text.size /= text_size_field_default then
					log (message_warning 
						 & "text size of field '" & to_string (meaning)
						 & "' invalid. Found " & to_string (size => text.size) 
						 & ". Expected " & to_string (size => text_size_field_default) & " !");
				end if;
				
				text.orientation := to_field_orientation (get_field_from_line (line,6));
				-- CS: check orientation.
				
				text.visible := to_field_visible (
					vis_in		=> get_field_from_line (line,7),
					schematic	=> false);
				-- CS: check visibility.

				text.alignment.horizontal := to_alignment_horizontal (get_field_from_line (line,8));
				-- CS: check hor aligment.
				text.alignment.vertical   := to_alignment_vertical  (get_field_from_line (line,9));
				-- CS: check vert aligment.
				text.style := to_text_style (style_in => get_field_from_line (line,9), text => false);
				-- CS: check style.
				
				-- NOTE: text.line_width assumes default (see et_general.ads) as no line width is provided here.
				return text;
			end read_field;

			procedure insert_component (
			-- NOTE: This is library related stuff.
			-- Updates the current library by inserting the component.
			-- If the component was inserted (should be) the comp_cursor points to the component
			-- for later inserting the units:
				key			: in type_full_library_name.bounded_string;
				components	: in out type_components.map) is
			begin -- insert_component

				-- Do a precheck of the text fields. -- CS most of the checks is already done in procedure read_field.
				check_text_fields;

-- 				-- For the logfile write the component name.
-- 				-- If the component contains more than one unit, write number of units.

				case tmp_appearance is
					when sch =>

						-- we insert into the given components list a new component
						type_components.insert(
							container	=> components,
							key			=> tmp_component_name, -- #PWR, #FLG 
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
								value			=> type_component_value.to_bounded_string (content (tmp_value)),
								commissioned	=> et_string_processing.type_date (content (tmp_commissioned)),
								updated			=> et_string_processing.type_date (content (tmp_updated)),
								author			=> type_person_name.to_bounded_string (content (tmp_author)),
								units_internal	=> type_units_internal.empty_map,
								units_external	=> type_units_external.empty_map
								)
							);
						
					when sch_pcb =>

						-- we insert into the given components list a new component
						type_components.insert(
							container	=> components,
							key			=> tmp_component_name, -- 74LS00
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> sch_pcb,
								prefix			=> tmp_prefix,
								value			=> type_component_value.to_bounded_string (content (tmp_value)),
								commissioned	=> et_string_processing.type_date (content (tmp_commissioned)),
								updated			=> et_string_processing.type_date (content (tmp_updated)),
								author			=> type_person_name.to_bounded_string (content (tmp_author)),
								units_internal	=> type_units_internal.empty_map,
								units_external	=> type_units_external.empty_map,

								variants		=> type_component_variants.empty_map,
								-- NOTE: kicad does not know about package variants (as EAGLE does). So the variants list here is empty.
								-- Instead we provide the so called package filter, which is empty for the moment.
								package_filter	=> type_package_filter.empty_set,
								datasheet		=> type_component_datasheet.to_bounded_string (content (tmp_datasheet)),
								purpose			=> type_component_purpose.to_bounded_string (content (tmp_purpose)),
								partcode		=> type_component_partcode.to_bounded_string (content (tmp_partcode)),
								bom				=> type_bom'value (content (tmp_bom))
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
							-- CS: provide details about the problem
						raise;
						
			end insert_component;

			
			procedure set_unit_cursor (libraries : in out type_libraries.map) is
			-- Sets the unit_cursor according to the current unit_id.
			-- If the unit_id is 0, the unit_cursor is not changed.
		
				procedure locate_unit (
				-- sets the unit_cursor
					key			: in type_component_name.bounded_string;
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
				
			
			procedure add_unit (libraries : in out type_libraries.map) is
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
					key			: in type_component_name.bounded_string;
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
			-- If the tmp_unit_id is 0, the symbol element is inserted into all units (except extra units).
			
			-- The kind of symbol element is given by parameter "element".
			-- The symbol properties are taken from the temporarily variables named tmp_draw_*.
						
				libraries	: in out type_libraries.map;
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
							--unit.symbol.ports.insert (key => tmp_draw_port_name, new_item => tmp_draw_port);
							unit.symbol.ports.append (tmp_draw_port);
							
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
					key			: in type_component_name.bounded_string;
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
			

			procedure set_text_placeholder_properties (libraries : in out type_libraries.map) is
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
					unit.symbol.reference	:= (type_text_basic (tmp_reference)		with meaning => reference);
					unit.symbol.value		:= (type_text_basic (tmp_value)			with meaning => value);
					unit.symbol.commissioned:= (type_text_basic (tmp_commissioned)	with meaning => commissioned);
					unit.symbol.updated		:= (type_text_basic (tmp_updated)		with meaning => updated);
					unit.symbol.author		:= (type_text_basic (tmp_author)		with meaning => author);

					case unit.symbol.appearance is
						when sch_pcb =>
							unit.symbol.packge		:= (type_text_basic (tmp_package)	with meaning => packge);
							unit.symbol.datasheet	:= (type_text_basic (tmp_datasheet)	with meaning => datasheet);
							unit.symbol.purpose		:= (type_text_basic (tmp_purpose)	with meaning => purpose);
							unit.symbol.partcode	:= (type_text_basic (tmp_partcode)	with meaning => partcode);
							unit.symbol.bom			:= (type_text_basic (tmp_bom)		with meaning => bom);
						when others => null;
					end case;
				end set;
				
				procedure locate_unit (
				-- Locates the unit indicated by unit_cursor.
					key			: in type_component_name.bounded_string;
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
			
		procedure read_draw_object (line : in type_fields_of_line) is
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
-- 				log_threshold : type_log_level := 2;
			begin
				log_indentation_up;
				log ("scope", log_threshold + 1);
				log_indentation_up;
				
				if unit = 0 then
					log ("common to all units", log_threshold + 1);
				else
					log ("unit" & type_unit_id'image(unit), log_threshold + 1);
				end if;
				
				log_indentation_down;
				log_indentation_down;
			end write_scope_of_object;

-- 			log_threshold : type_log_level := 2;
			
		begin -- read_draw_object
			log ("draw object", level => log_threshold + 1);
			log_indentation_up;

			-- At a certain log level we report the bare line of a draw object as it is:
			log (to_string (line), log_threshold + 2);
			
			case type_library_draw'value (field (line,1)) is
				when P => -- polyline
					log ("polyline", level => log_threshold + 1);
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
					log (to_string (line), level => log_threshold + 1);
					
					tmp_unit_id := to_unit_id (field (line,3));
					write_scope_of_object (tmp_unit_id);

					-- compose polyline
					tmp_draw_polyline := to_polyline (line);

					-- add polyline to unit
					add_symbol_element (component_libraries, polyline);
					
				when S => -- rectangle
					log ("rectangle", level => log_threshold + 1);
					-- A rectangle is defined by a string like "S -40 -100 40 100 0 1 10 N"
					-- field meaning;
					-- #2..5 : start point -40/-100   end point 40/100
					-- #6 : 0 -> common to all units, otherwise unit id it belongs to
					-- #7 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
					-- #8 : line width
					-- #9 : fill style N/F/f no fill/foreground/background

					log (to_string (line), level => log_threshold + 1);					
					
					tmp_unit_id := to_unit_id (field (line,6));
					write_scope_of_object (tmp_unit_id);

					-- compose rectangle
					tmp_draw_rectangle := to_rectangle (line);

					-- add rectangle to unit
					add_symbol_element (component_libraries, rectangle);
					
				when C => -- circle
					log ("circle", level => log_threshold + 1);
					-- A circle is defined by a string like "C 0 0 112 0 1 23 N"
					-- field meaning:
					--  #2..3 : center (x/y)
					--  #4 : radius
					--  #5 : 0 -> common to all units, otherwise unit id it belongs to
					--  #6 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
					--  #7 : line width (23)
					--  #8 : fill style N/F/f no fill/foreground/background

					log (to_string (line), level => log_threshold + 1);
					
					tmp_unit_id := to_unit_id (field (line,5));
					write_scope_of_object (tmp_unit_id);

					-- compose circle
					tmp_draw_circle := to_circle (line);
					
					-- add circle to unit
					add_symbol_element (component_libraries, circle);
					
				when A => -- arc
					log ("arc", level => log_threshold + 1);
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

					log (to_string (line), level => log_threshold + 1);
					
					tmp_unit_id := to_unit_id (field (line,7));
					write_scope_of_object (tmp_unit_id);

					-- compose arc
					tmp_draw_arc := to_arc (line);
					
					-- add arc to unit
					add_symbol_element (component_libraries, arc);

				when T => -- text
					log ("text", level => log_threshold + 1);
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

					log (to_string (line), level => log_threshold + 1);
					
					tmp_unit_id := to_unit_id (field (line,7));
					write_scope_of_object (tmp_unit_id);

					-- compose text
					tmp_draw_text := to_text (line);
					
					-- add text to unit
					add_symbol_element (component_libraries, text);
					
				when X => -- port
					log ("port", level => log_threshold + 1);
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

					log (to_string (line), level => log_threshold + 1);
					
					tmp_unit_id := to_unit_id (field (line,10));
					write_scope_of_object (tmp_unit_id);

					-- compose port
					tmp_draw_port := to_port (line);
-- 					tmp_draw_port_name := type_port_name.to_bounded_string (field (line,2));

					-- If this is a unit specific port it gets added to the unit. If it applies for the
					-- whole component, we create an extra unit and insert it there. An extra unit is
					-- created ONLY ONCE. Successive unit-wide ports are added there.
					-- An extra unit always has the add level "request" since it harbors the supply ports.
					-- When adding a port, tmp_unit_id is always greater zero.
					if tmp_unit_id > 0 then
						-- add unit specific port to unit

						--log ("unit id " & type_unit_id'image (tmp_unit_id) , level => log_threshold);
						add_symbol_element (component_libraries, port);
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
						add_symbol_element (component_libraries, port);
					end if;
			end case;

			log_indentation_down;
		end read_draw_object;

		procedure add_footprint (line : in type_fields_of_line) is
		-- Reads the proposed footprint and adds it to the package filter of the current component.
-- 			log_threshold : type_log_level := 2;
			fp : type_package_proposal.bounded_string;

			function field (line : in type_fields_of_line; position : in positive) return string renames
				et_string_processing.get_field_from_line;
			
			procedure do_it (libraries : in out type_libraries.map) is
			-- Adds the footprint finally.
				procedure insert_footprint (
					key			: in type_component_name.bounded_string;
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
			
		begin
			log ("footpint/package filter", level => log_threshold + 1);
			log_indentation_up;

			fp := type_package_proposal.to_bounded_string (field (line,1));
			log (type_package_proposal.to_string(fp), level => log_threshold + 1);

			do_it (component_libraries);
			
			log_indentation_down;
		end add_footprint;
		

		procedure read_field (line : in type_fields_of_line) is
		-- Reads the kicad text field of a component in a temporarily variable.
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

			-- CS: flags that indicate the finding of a field. raise alarm if field missing. mind appearance
			
		begin -- read_field
			
			-- read text fields from a component library (thats why scheamtic => false)
			case to_text_meaning (line => line, schematic => false) is

				-- If we have the reference field like "F0 "U" 0 50 50 H V C CNN"
				when reference =>
								
					-- CS: Do a cross check of prefix and reference -- "U" 
					-- CS: why this redundance ? Ask the kicad makers...
					if strip_quotes (field (line,2)) = type_component_prefix.to_string (tmp_prefix) then
						null; -- fine
					else
						log (message_warning & affected_line(line) & ": prefix vs. reference mismatch !");
					end if;

					tmp_reference := read_field (meaning => reference);
					-- for the log:
					write_text_properies (type_text (tmp_reference)); -- actuals: text & indentation

				-- If we have a value field like "F1 "74LS00" 0 -100 50 H V C CNN"
				when value =>
				
					tmp_value := read_field (meaning => value);
					-- for the log:
					write_text_properies (type_text (tmp_value)); -- actuals: text & indentation

				-- If we have a footprint field like "F2 "" 0 -100 50 H V C CNN"
				when packge =>
				
					tmp_package := read_field (meaning => packge);
					-- for the log:
					write_text_properies (type_text (tmp_package)); -- actuals: text & indentation

				-- If we have a datasheet field like "F3 "" 0 -100 50 H V C CNN"
				when datasheet =>
				
					tmp_datasheet := read_field (meaning => datasheet);
					-- for the log:
					write_text_properies (type_text (tmp_datasheet)); -- actuals: text & indentation

				-- Other mandatory fields like function and partcode are detected by F4 and F5 
				-- (not by subfield #10 !) So F4 enforces a function, F5 enforces a partcode.
				
				-- If we have a purpose field like "F9 "" 0 -100 50 H V C CNN" "purpose",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when purpose =>
				
					if to_lower (strip_quotes (field (line,10))) = to_lower (type_text_meaning'image (purpose)) then
						tmp_purpose := read_field (meaning => purpose);
						-- for the log:
						write_text_properies (type_text (tmp_purpose)); -- actuals: text & indentation
						-- basic_text_check(fnction); -- CS
					else
						invalid_field(line);
					end if;

				-- If we have a partcode field like "F7 "" 0 -100 50 H V C CNN" "partcode",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when partcode =>
				
					if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (partcode)) then
						tmp_partcode := read_field (meaning => partcode);
						-- for the log:
						write_text_properies (type_text (tmp_partcode)); -- actuals: text & indentation
						-- basic_text_check(partcode); -- CS
					else
						invalid_field(line);
					end if;

				-- If we have a "commissioned" field like "F4 "" 0 -100 50 H V C CNN" "commissioned",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when commissioned =>
				
					if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (commissioned)) then
						tmp_commissioned := read_field (meaning => commissioned);
						-- for the log:
						write_text_properies (type_text (tmp_commissioned)); -- actuals: text & indentation
						-- basic_text_check(commissioned); -- CS
					else
						invalid_field(line);
					end if;

				-- If we have an "updated" field like "F5 "" 0 -100 50 H V C CNN" "updated",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when updated =>
				
					if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (updated)) then
						tmp_updated := read_field (meaning => updated);
						-- for the log:
						write_text_properies (type_text (tmp_updated)); -- actuals: text & indentation
						-- basic_text_check(updated); -- CS
					else
						invalid_field(line);
					end if;

				-- If we have an "author" field like "F6 "" 0 -100 50 H V C CNN" "author",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when author =>
				
					if to_lower (strip_quotes(field (line,10))) = to_lower (type_text_meaning'image (author)) then
						tmp_author := read_field (meaning => author);
						-- for the log:
						write_text_properies (type_text (tmp_author)); -- actuals: text & indentation
						-- basic_text_check(author); -- CS
					else
						invalid_field(line);
					end if;

				-- If we have a "bom" field like "F8 "" 0 -100 50 H V C CNN" "bom",
				-- we test subfield #10 against the prescribed meaning. If ok the field is read like
				-- any other mandatory field (see above). If invalid, we write a warning. (CS: should become an error later)
				when bom =>
				
					if to_lower (strip_quotes (field (line,10))) = to_lower (type_text_meaning'image (bom)) then
						tmp_bom := read_field (meaning => bom);
						-- for the log:
						write_text_properies (type_text (tmp_bom)); -- actuals: text & indentation
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

						log (text => to_string (line), level => 3);
						
						if not component_entered then 
							
							if get_field_from_line(line,1) = et_kicad.def then
								component_entered := true;

								init_temp_variables;
								
								-- Since we are reading the fields, we set the active_section to "fields"
								active_section := fields;

								-- The commponent header provides the first component properties:
								tmp_component_name := et_libraries.type_component_name.to_bounded_string (get_field_from_line(line,2)); -- 74LS00
								et_libraries.check_component_name (tmp_component_name);
								
								-- for the log:
								log (get_field_from_line(line,2), log_threshold + 1); -- 74LS00

								-- From the header we extract some basic information about the component:
								
								-- The line it is about looks like:  DEF 74LS00 U 0 30 Y Y 4 F N
								-- The fields meaning is as follows:
								--  #2 : name, like 74LS00
								--  #3 : prefix, like U
								--  #4 : unknown -- CS: what is it good for ?
								--  #5 : pin name position offset of supply pins, if "place pin names inside" is off. the offset assumes zero
								--  #6 : show pin/pad number Y/N,
								--  #7 : show pin/port name Y/N,
								--  #8 : units total, -- like 4
								--  #9 : all units not interchangeable L (otherwise F), (similar to swap level in EAGLE)
								--  #10: power symbol P (otherwise N)

								tmp_prefix := type_component_prefix.to_bounded_string (get_field_from_line (line,3)); -- U
								tmp_port_name_offset	:= mil_to_distance (mil => get_field_from_line (line,5), warn_on_negative => false); -- relevant for supply pins only
								tmp_pin_name_visible	:= to_pin_visibile (get_field_from_line (line,6));
								tmp_port_name_visible	:= to_port_visibile (get_field_from_line (line,7));
								
								-- Get number of units and set swap level as specified in field #9.
								-- Swap level assumes default if only one unit available.
								tmp_units_total := type_units_total'value (get_field_from_line (line,8));
								if tmp_units_total > 1 then
									log_indentation_up;
									log ("with" & type_units_total'image (tmp_units_total) & " units", level => log_threshold + 1);

									-- From the "interchangeable" flag we set the component wide swap level. It applies for 
									-- all units of the component (except extra units):
									tmp_unit_swap_level := to_swap_level (get_field_from_line(line,9));
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
							if get_field_from_line(line,1) = et_kicad.enddef then
								component_entered := false;

								-- Set placeholders (reference, value, commissioned, ...) in internal units.
								-- The placeholder properties are known from the field-section.
								-- The placeholder properties apply for all units.
								set_text_placeholder_properties (component_libraries);
								
								-- log component properties
								if log_level >= 2 then
									et_libraries.write_component_properties (component => comp_cursor);
								end if;
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
										if get_field_from_line(line,1) = et_kicad.fplist then
											
											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
												container	=> component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;
											
											active_section := footprints;
											log ("footprint filter begin", level => log_threshold + 2);

										elsif get_field_from_line(line,1) = et_kicad.draw then

											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
												container	=> component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;

											active_section := draw;
											log ("draw begin", level => log_threshold + 2);
										else
											read_field (line);
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
										if get_field_from_line(line,1) = et_kicad.endfplist then
											active_section := none;
											log ("footprint filter end", level => log_threshold + 2);
										else
											-- Process lines:
											add_footprint (line);
										end if;

									when draw =>
										-- Here we read the drawing list where lines, arcs and pins are.
										
										-- As long as the footer of the list (ENDDRAW) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- thate this subsection has been processed.
										if get_field_from_line (line,1) = et_kicad.enddraw then
											active_section := none;
											log ("draw end", level => log_threshold + 2);
										else
											-- Process lines:
											read_draw_object (line);
										end if;
										
									when none =>
										-- If no subsection is being processed, we wait for the "draw" header (DRAW)
										-- and set the active_section accordingly.
										-- NOTE #2: the active section "fields" is not set here but when the fields are read (see NOTE #1)
										if get_field_from_line (line,1) = et_kicad.draw then
											active_section := draw;
											log ("draw begin", level => log_threshold + 2);
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

		
	begin -- read_components_libraries
		et_schematic.reset_library_cursor (project_lib_cursor);
		
		-- If there were no libraries in the project file, there is nothing to do but writing a warning:
		if et_schematic.number_of_libraries = 0 then
			log (message_warning & "no component libraries defined in project file !");
		else
			log (text => "Loading component libraries ...", console => true);
			log_indentation_up;
			
			-- We loop in the list of project libraries:
			while project_lib_cursor /= type_full_library_names.no_element loop

				-- Set the library to be read:
				lib_file_name := element (project_lib_cursor);

				-- log library file name
				log (to_string (lib_file_name), console => true, level => log_threshold);
				
				if exists ( to_string (lib_file_name)) then
					open (
						file => library_handle,
						mode => in_file,
						name => to_string (lib_file_name));

					-- Since the full libary file name (incl. path) is known, we insert an empty
					-- library in the list of component libraries.
					-- After that the lib_cursor points to the latest inserted library.
					type_libraries.insert (
						container	=> component_libraries,
						key			=> lib_file_name, -- full library file name (incl. path) like "/home/user/lib/my_lib.lib"
						new_item	=> type_components.empty_map,
						position	=> lib_cursor,
						inserted	=> lib_inserted
						);

					-- this is a double check. should never fail.
					if lib_inserted then
						-- Now we read the library file and add components
						-- to the library pinted to by lib_cursor:
						set_input (library_handle);
						read_library;
					else
						log_indentation_reset;
						log (message_error & to_string (lib_file_name) & " already in component libraries !",
							 console => true);
						raise constraint_error;
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
	

	procedure import_design is
		--use et_import.type_schematic_file_name;
		use et_libraries.type_library_directory;
		use et_schematic;

		function field (line : in type_fields_of_line; position : in positive) return string renames
			et_string_processing.get_field_from_line; -- CS: apply in read_schematic
		
		list_of_submodules : type_submodule_names_extended;
		
		top_level_schematic	: type_schematic_file_name.bounded_string;
		current_schematic	: type_schematic_file_name.bounded_string;

		net_id : natural := 0; -- for counting name-less nets (like N$1, N$2, N$3, ...)

		log_threshold : type_log_level := 1;

		package stack_of_sheet_lists is new et_general.stack_lifo (max => 10, item => type_submodule_names_extended);
        use stack_of_sheet_lists;
		
		function read_project_file return type_schematic_file_name.bounded_string is
		-- Reads the project file in terms of LibDir and LibName. 
		-- LibDir is stored in variable lib_dir.
		-- Project library names are stored in project_libraries.
		-- Returns the name of the top level schematic file.
			line : type_fields_of_line;
			
			use type_project_file_name;
			use et_libraries;
			use et_schematic;

		-- CS: move to spec begin
            section_eeschema_entered : boolean := false;
            section_eeschema_libraries_entered : boolean := false;            

            procedure clear_section_entered_flags is
            begin
                section_eeschema_entered := false;
                section_eeschema_libraries_entered := false;
            end clear_section_entered_flags;
		-- CS: move to spec end
			
		begin -- read_project_file
			log_indentation_reset;
			log (text => "reading project file " 
				 & compose (
					name		=> et_schematic.type_project_name.to_string (et_schematic.project_name), 
					extension	=> file_extension_project) & " ...");
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
							name		=> et_schematic.type_project_name.to_string (et_schematic.project_name), 
							extension	=> file_extension_project)
				);
			set_input (project_file_handle);
			
			while not end_of_file loop

				-- Save a line in variable "line" (see et_string_processing.ads)
				line := read_line(
							line => get_line,
							comment_mark => "#", -- use constant comment_mark
							number => ada.text_io.line(current_input),
							ifs => latin_1.equals_sign); -- fields are separated by equals sign (=)

				case field_count(line) is
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

							-- Get root path to libraries (LibDir) and store it in lib_dir (see et_kicad.ads)
							-- CS: currently we assume only one path here. Provide procedure that sets lib_dir and checks
							-- deviations.
							if field (line,1) = project_keyword_library_directory then
								lib_dir := to_bounded_string (field (line,2));

								-- For the log write something like "LibDir ../../lbr"
								log (text => project_keyword_library_directory & " " & et_libraries.to_string (lib_dir));
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
								log (text => field (line,1) & " " & field (line,2));
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
					name		=> et_schematic.type_project_name.to_string (project_name), 
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
		procedure set_e (segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
		procedure set_s (segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;
		procedure set_picked (segment : in out type_wild_net_segment ) is begin segment.picked := true; end set_picked;

			
		-- An anonymous strand is a list of net segments that are connected with each other (by their start or end points).
		-- The anonymous strand gets step by step more properties specified: name, scope and some status flags:
		package type_anonymous_strand is new doubly_linked_lists ( -- CS: move to spec
			element_type => type_net_segment);

		-- This is an intermediate type used for handling anonymous strands:
		type type_anonymous_strand_extended is record
			segments 	: type_anonymous_strand.list;	-- the net segments
			name 		: type_net_name.bounded_string; -- the name (derived from net labels)
			scope 		: type_scope_of_net := type_scope_of_net'first;	-- the scope (derived from net labels)
			processed	: boolean := false;				-- set once a label has been found on the net
			sorted		: boolean := false;				-- set once sorted out while sorting named nets -- CS: remove. obsolete
		end record;

		package type_anonymous_strands is new doubly_linked_lists (
			element_type => type_anonymous_strand_extended);
		
		-- When sorting named strands, this procedure sets the "sorted" flag of the anonymous strand.
		procedure set_sorted (anon_net : in out type_anonymous_strand_extended) is 
			begin anon_net.sorted := true; end set_sorted;
		
			
		procedure init_temp_variables is
		begin
			-- clear "field found" flags
			tmp_component_text_reference_found		:= false;
			tmp_component_text_value_found			:= false;
			tmp_component_text_commissioned_found	:= false;
			tmp_component_text_updated_found		:= false;
			tmp_component_text_author_found			:= false;
			tmp_component_text_packge_found			:= false;
			tmp_component_text_datasheet_found		:= false;
			tmp_component_text_purpose_found		:= false;
			tmp_component_text_partcode_found		:= false;
			
			tmp_component_text_packge.content := et_libraries.type_text_content.to_bounded_string("");
			-- CS: init text properties ?
			-- CS: init remaining tmp vars ?

		end init_temp_variables;

		procedure check_text_fields (log_threshold : in type_log_level) is 
		-- NOTE: This is schematic related !
		-- Tests if any "field found" flag is still cleared and raises an alarm in that case.
		-- Perfoms a plausibility and syntax check on the text fields before they are used to 
		-- assemble and insert the component into the component list of the module.
		-- This can be regarded as a kind of pre-check.
		-- CS: check text size and width
			use et_coordinates;
		
			procedure missing_field (m : in et_libraries.type_text_meaning) is 
			begin
				log_indentation_reset;
				log (
					text => message_error 
						& "component " & to_string (tmp_component_reference) 
						& latin_1.space
						& to_string (position => tmp_component_position)
						& latin_1.lf
						& "text field '" & et_libraries.to_string (m) & "' missing !",
					console => true);
				
				raise constraint_error;
			end missing_field;

			commissioned, updated : et_string_processing.type_date;

			use et_libraries;
		begin -- check_text_fields
			log_indentation_up;

			-- write precheck preamble
			log ("component " 
				& to_string(tmp_component_reference) & " prechecking fields ...", level => log_threshold);

-- 			log_indentation_up;
-- 			log ("precheck", log_threshold + 1);
			log_indentation_up;
			
			-- reference
			log ("reference", level => log_threshold + 1);
			if not tmp_component_text_reference_found then
				missing_field (et_libraries.reference);
			else
				-- verify tmp_component_text_reference equals tmp_component_reference. @kicad: why this redundance ?
				-- KiCad stores redundant information on the component reference as in this example;

				-- $Comp
				-- L 74LS00 IC1 <- tmp_component_reference
				-- U 1 1 59969711
				-- P 4100 4000
				-- F 0 "IC1" H 4100 4050 50  0000 C BIB <- tmp_component_text_reference
				
				if et_schematic.to_string (tmp_component_reference) /= et_libraries.content (tmp_component_text_reference) then
					log_indentation_reset;
					log (message_error & " reference mismatch !");
					log (et_schematic.to_string (tmp_component_reference) & " vs " & et_libraries.content(tmp_component_text_reference));
					raise constraint_error;
				end if;

				-- CS: check if prefix meets certain conventions
			end if;

			-- value
			log ("value", level => log_threshold + 1);
			if not tmp_component_text_value_found then
				missing_field (et_libraries.value);
			else
				-- depending on the component reference (like R12 or C9) the value must meet certain conventions:
				if not et_libraries.component_value_valid (
					value => et_libraries.type_component_value.to_bounded_string (
						et_libraries.content (tmp_component_text_value)), -- the content of the value field like 200R or 10uF
					reference => tmp_component_reference) -- the component reference such as R4 or IC34
					then raise constraint_error;
				end if;
			end if;

			-- commissioned
			log ("commissioned", level => log_threshold + 1);
			if not tmp_component_text_commissioned_found then
				missing_field (et_libraries.commissioned);
			else
				-- The commissioned time must be checked for plausibility and syntax.
				-- The string length is indirecty checked on converting the field content to derived type_date.
				commissioned := et_string_processing.type_date (et_libraries.content (tmp_component_text_commissioned));
				if not et_string_processing.date_valid (commissioned) 
					then raise constraint_error;
				end if;
			end if;

			-- updated
			log ("updated", level => log_threshold + 1);
			if not tmp_component_text_updated_found then
				missing_field (et_libraries.updated);
			else
				-- The update time must be checked for plausibility and syntax.
				-- The string length is indirecty checked on converting the field content to derived type_date.					
				updated := et_string_processing.type_date (et_libraries.content (tmp_component_text_updated));
				if not et_string_processing.date_valid (updated) 
					then raise constraint_error;
				end if;

				-- make sure the update was later (or at the same time as) the commission date
				check_updated_vs_commissioned (commissioned, updated);

			end if;

			-- author
			log ("author", level => log_threshold + 1);
			if not tmp_component_text_author_found then
				missing_field (et_libraries.author);
			else
				null;
				-- CS: check content of tmp_component_text_author
			end if;

			-- If we are checking fields of a real component there are more 
			-- fields to be checked. If it is about a virtual component, those 
			-- fields are ignored and thus NOT checked:
			case tmp_component_appearance is
				when sch_pcb =>
						
					-- package
					log ("package/footprint", level => log_threshold + 1);
					if not tmp_component_text_packge_found then
						missing_field (et_libraries.packge);
					else
						null;
						-- CS: check content of tmp_component_text_packge
					end if;

					-- datasheet
					log ("datasheet", level => log_threshold + 1);
					if not tmp_component_text_datasheet_found then
						missing_field (et_libraries.datasheet);
					else
						null;
						-- CS: check content of tmp_component_text_datasheet
					end if;

					-- partcode
					log ("partcode", level => log_threshold + 1);
					if not tmp_component_text_partcode_found then
						missing_field (et_libraries.partcode);
					else
						null;
						-- CS: check content of tmp_component_text_partcode
					end if;
					
					-- purpose
					log ("purpose", level => log_threshold + 1);
					if not tmp_component_text_purpose_found then
						missing_field (et_libraries.purpose);
					else
						null;
						-- CS: check content of tmp_component_text_fnction
					end if;

					-- bom
					log ("bom", level => log_threshold + 1);
					if not tmp_component_text_bom_found then
						missing_field (et_libraries.bom);
					else
						null;
						-- CS: check content of tmp_component_text_bom
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
						text => message_error & "invalid field in component " & et_schematic.to_string (tmp_component_reference)
							& " at " & to_string (position => tmp_component_position),
						console => true);
					-- CS: evaluate prog position and provided more detailled output
					raise constraint_error;

		end check_text_fields;
			

			
		function read_schematic (
			current_schematic	: in type_schematic_file_name.bounded_string;
			log_threshold		: in type_log_level)
			return type_submodule_names_extended is
		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
        -- they will be returned in list_of_submodules. Otherwise the returned list is empty.

			list_of_submodules : type_submodule_names_extended; -- list to be returned
			name_of_submodule_scratch : et_coordinates.type_submodule_name.bounded_string; -- temporarily used before appended to list_of_submodules

			use et_string_processing;
			--use et_libraries;
		
			line : et_string_processing.type_fields_of_line; -- the line of the schematic file being processed
		
			sheet_file : type_schematic_file_name.bounded_string;
			sheet_count_total, sheet_number_current : positive;


			-- When reading the sheet header, its content goes into sheet_header.
			-- This is stuff like:
			--     LIBS:nucleo_core-rescue
			--     LIBS:power
			--     LIBS:bel_connectors_and_jumpers
			--     LIBS:bel_primitives
			--     LIBS:bel_stm32
			--     LIBS:nucleo_core-cache
			--     EELAYER 25 0
			--     EELAYER END
			-- The library names here are of no importance. So it is sufficient to just store the 
			-- library names in the sheet header.
            sheet_header : type_sheet_header;

			schematic_headline_processed : boolean := false;
            
			tmp_wild_simple_labels	: type_simple_labels.list;
            tmp_wild_tag_lables 	: type_tag_labels.list;
			wild_segments			: type_wild_segments.list;
			tmp_junctions			: type_junctions.list;

			-- In the first stage, all net segments of this sheet go into a wild collection of segments.
			-- Later they will be sorted and connected by their coordinates (start and and points)
			segment_count	: count_type := 0; -- holds the total number of segments within a sheet
			
			anonymous_strand : type_anonymous_strand_extended;

			-- The list of anonymous strands. Procedure add_strand_to_anonymous_strands uses 
			-- this container for temporarily storage of anonymous strands.
			anonymous_strands : type_anonymous_strands.list; 

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
					type_anonymous_strand.append (anonymous_strand.segments, scratch);
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
-- 				for i in 1..segment_count loop
					if cursor /= segment_cursor then -- skip the given segment
						line_start := type_wild_segments.element (cursor).coordinates_start;
						line_end   := type_wild_segments.element (cursor).coordinates_end;
						s  := type_wild_segments.element (cursor).s;
						e  := type_wild_segments.element (cursor).e;
						untouched := not (s or e); -- neither s nor e set
						--fully_processed := s and e;

						if untouched then 
							--put(et_import.report_handle,"probe untouched segment: ");
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

				-- No untouched segment found.
				-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				cursor := wild_segments.first;
				--for i in 1..segment_count loop
				while cursor /= no_element loop
					--if i /= id then -- skip the given segment
					if cursor /= segment_cursor then
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
				
			-- The first label found on the net dictates the strand name.
			-- Other labels on the strand are checked for their name only. 
			-- If the name differs from the strand name set earlier, an error is output.
			-- If scope of strands are contradicting, error is output.

			-- The kind of net label (simple, hierarchic, global) defines the scope of the strand (and finally the net).
			-- Net labels sitting on a segment, are added to the list of labels of that segment.
			
			-- Strands without label remain anonymous by using the notation "N$". 

				use et_coordinates;
			
				ls  :	type_net_label_simple;
				lt  : 	type_net_label_tag;				
				anon_strand_a, anon_strand_b : type_anonymous_strand_extended;
				segment	: type_net_segment;
				lls : 	type_simple_labels.list;
				llt : 	type_tag_labels.list;
			
				strand 		: type_strand_named;
				net_name	: type_net_name.bounded_string;
				
				function label_sits_on_segment (label : in type_net_label; segment : in type_net_segment) return boolean is
					sits_on_segment : boolean := false;
					d : et_geometry.type_distance_point_from_line;

					--use et_libraries;
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

				use type_anonymous_strand;
				-- the segment cursor points to the segment being processed
				segment_cursor : type_anonymous_strand.cursor; 

				use type_anonymous_strands;
				
				-- the strand cursor points to the anonymous strand being processed
				strand_cursor	: type_anonymous_strands.cursor := anonymous_strands.first;
				strand_cursor_b	: type_anonymous_strands.cursor;

				use type_simple_labels;
				simple_label_cursor	: type_simple_labels.cursor; -- points to the simple label being processed

				use type_tag_labels;
				tag_label_cursor	: type_tag_labels.cursor; -- points to the tag label being processed

				use type_net_name;
				
			begin -- associate_net_labels_with_anonymous_strands
				log_indentation_up;
				
				-- This does only make sense if there are strands at all:
				if not is_empty (anonymous_strands) then
					log (text => "associating net labels with strands ...");
					
					-- Loop in list of anonymous strands, get a (non-processed-yet) strand, loop in list of segments and 
					-- find a (non-processed-yet) net label that sits on the net segment. If label sits on segment:
					--  - assume label text as name of strand (and check other labels of the anonymous strand)
					--
					--  - mark label as processed
					--  - update/replace label in tmp_wild_simple_labels or tmp_wild_tag_lables
					--
					--  - Collect label in temporarily list of labels.
					--
					--  - Mark anonymous strand as processed. This indicates that the strand has a name (given by a label).
					--    Non-Processed strands are those without a label.
					--  - update/replace anonymous strand in anonymous_strands
					while strand_cursor /= type_anonymous_strands.no_element loop -- cursor already reset on declaration (see above)
						anon_strand_a := element (strand_cursor); -- get anonymous strand
						
						--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": "); -- CS: log ?
						if not anon_strand_a.processed then -- skip already processed nets

							-- reset segment cursor to begin of segment list of the anonymous net
							segment_cursor := anon_strand_a.segments.first;
							while segment_cursor /= type_anonymous_strand.no_element loop -- loop for each segment in anonymous strand anon_strand_a
								segment := anon_strand_a.segments (segment_cursor);
								--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s); -- CS: log ?
								
								-- Loop in list of simple labels:
								if not is_empty (tmp_wild_simple_labels) then -- do that if there are simple labels at all
									--put_line(" simple labels ..."); -- CS: log ?
									simple_label_cursor := tmp_wild_simple_labels.first; -- reset label cursor
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

												-- The first matching simple label dictates the strand name. 
												-- If other labels with text differing from strand name found, output warning.
												if type_net_name.length (anon_strand_a.name) = 0 then -- If this is the first matching label

													-- assume the label text as strand name.
													anon_strand_a.name := ls.text; 

													-- since this is a simple label, the scope of the strand is local
													anon_strand_a.scope := local;
												else
													-- If label text is different from previously assigned strand name:
													if not type_net_name."=" (anon_strand_a.name, ls.text) then

														-- for the console a short message
														put_line (standard_output, message_error & "Net label conflict !");

														-- for the log, some more information
														log_indentation_reset;
														log (message_error 
															 & "Net " & type_net_name.to_string (anon_strand_a.name) & " has contradicting label " 
															 & "at " & to_string (position => ls.coordinates) & " !");
														raise constraint_error;
													end if;
												end if;

												-- mark simple label as processed and update/replace it in tmp_wild_simple_labels
												ls.processed := true;
												type_simple_labels.replace_element(
													container => tmp_wild_simple_labels,
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
									type_anonymous_strand.replace_element (
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
								--if type_tag_labels.length (tmp_wild_tag_lables) > 0 then -- do that if there are tag labels at all
								-- if length (tmp_wild_tag_lables) > 0 then -- do that if there are tag labels at all
								if not is_empty (tmp_wild_tag_lables) then -- do that if there are tag labels at all
									--put_line(" hierarchic and global labels ...");	 -- CS: log ?

									tag_label_cursor := tmp_wild_tag_lables.first; -- reset label cursor
									--for l in 1..type_tag_labels.length (tmp_wild_tag_lables) loop 
									while tag_label_cursor /= type_tag_labels.no_element loop
										--lt := type_tag_labels.element (tmp_wild_tag_lables, positive(l)); -- get tag label
										lt := element (tag_label_cursor); -- get tag label
										
										if not lt.processed then								
											if label_sits_on_segment (label => type_net_label (lt), segment => segment) then

-- 												put_line(et_import.report_handle," tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 												type_grid'image(lt.coordinates.x) & "/" &
-- 												trim(type_grid'image(lt.coordinates.y),left));

												if log_level >= log_threshold + 1 then
													log_indentation_up;
													log ("label at " & to_string (label => type_net_label (lt), scope => xy));
													log_indentation_down;
												end if;

-- 												write_message(
-- 													file_handle => et_import.report_handle,
-- 													text => "tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 															type_grid'image(lt.coordinates.x) & "/" &
-- 															trim(type_grid'image(lt.coordinates.y),left),
-- 													identation => 3);

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
															log_indentation_reset;
															log (message_error
																& "local net " & type_net_name.to_string (anon_strand_a.name) 
																& " has a hierarchic or global label at " 
																& to_string (position => lt.coordinates) & " !");
															raise constraint_error;
														end if;
														
													when hierarchic => -- strand has been marked as "hierarchic" already. no global label allowed !
														if lt.global then
															log_indentation_reset;
															log (message_error
																& "hierarchic net " & type_net_name.to_string (anon_strand_a.name) 
																& " has a global label at " 
																& to_string (position => lt.coordinates) & " !");
															raise constraint_error;
														end if;

													when global => -- strand has been marked as "global" already. no hierarchic label allowed !
														if lt.hierarchic then
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

													-- CS: check for contradicting scope
												end if;

												-- mark tag label as processed and update/replace it in tmp_wild_tag_lables
												lt.processed := true;
												type_tag_labels.replace_element(
													container => tmp_wild_tag_lables,
													--index => positive(l),
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
									type_anonymous_strand.replace_element(
										container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
										position => segment_cursor,
										new_item => segment); -- the updated segment

									-- Clean up: Purge temporarily list of tag labels for next spin.
									type_tag_labels.clear (llt);

									-- Update/replace anonymous net in anonymous_nets.
									type_anonymous_strands.replace_element (
										container => anonymous_strands, -- the list of anonymous strands
										--index => positive(n), -- the anonymous net id
										position => strand_cursor,
										new_item => anon_strand_a); -- the updated anonymous net
								end if;

								next (segment_cursor); -- advance segment cursor
							end loop;
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					-- Sort anonymous strands (they do not have any labels).
					-- Anonymous strands have no name -> "processed" flag is still cleared.
					-- As placeholder for the name we use the notation "N$n" where n is an index (derived from the element id in anonymous_strands)
					-- Their scope is strictly "local".
					-- We us an intermediate variable "strand" for transfer to the module netlist.
					-- NOTE: Even if a strand has no name at this stage, it may receive a name after netlist generation.
					log (text => "sorting name-less strands ... NOTE: Names may change after netlist generation.");
					log_indentation_up;

					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get anonymous strand

						if not anon_strand_a.processed then

							-- build temporarily net with a name like N$542
							net_id := net_id + 1; -- increment net id. net_id applies for the whole design. see declarations of procedure import_design
							net_name := type_net_name.to_bounded_string (
								anonymous_net_name_prefix & trim (natural'image (net_id), left));

							log (type_net_name.to_string (net_name), level => 2);
							
							strand.name := net_name;
							strand.scope := local;

							log_indentation_up;
							log ("scope " & type_scope_of_net'image (strand.scope) & " with segments", level => 2);
							
							-- append segments to strand
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous net
							while segment_cursor /= type_anonymous_strand.no_element loop -- loop for each segment of anonymous strand anon_strand_a
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
							--set_module (strand.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
							set_module (strand.coordinates, to_submodule_name (current_schematic));
							set_path (strand.coordinates, path_to_submodule);
							set_sheet (strand.coordinates, sheet_number_current);

							-- set x,y coordinates (lowest available on the sheet)
							set (strand.coordinates, to_coordinates (lowest_xy (strand)));
                            
							-- insert strand in module, then purge strand.segments for next spin
							log ("inserting strand in module ...", level => 3);
							add_strand (strand);

							type_net_segments.clear (strand.segments);
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;
					
					log_indentation_down;
					
					-- Sort strands with label. Those strands have the "processed" flag set.
					-- NOTE: Even if a strand has a dedicated name in this stage, it may receive a name after netlist generation by force.
					-- Power-out ports may overwrite the strand name. As this would be a design error, the operator will 
					-- be notified and warned about such violations on netlist generation.
					log (text => "sorting named strands ...");
					log_indentation_up;
					
					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get a strand

						if anon_strand_a.processed and not anon_strand_a.sorted then -- it must have a name AND it must not be sorted yet

							log (type_net_name.to_string (anon_strand_a.name), level => 2);
							
							strand.name := anon_strand_a.name;
							strand.scope := anon_strand_a.scope;

							log_indentation_up;
							log ("scope " & type_scope_of_net'image (strand.scope) & " with segments", level => 2);

							-- append segments to strand
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous strand
							while segment_cursor /= type_anonymous_strand.no_element loop -- loop for each segment of anonymous_strand "a"
								segment := element (segment_cursor); -- get segment
								type_net_segments.append (container => strand.segments, new_item => segment);
								
								if log_level >= 2 then
									--write_coordinates_of_segment (segment => segment);
									log (to_string (segment => segment, scope => xy));
								end if;
								
								next (segment_cursor);
							end loop;

							-- Look for other anonymous strands with the same name (anon_strand_a.name). 
							-- Start searching from the position of strand_cursor on using strand_cursor_b.
							-- If strand_cursor already points the last strand in anonymous_strands do nothing.
							-- Mark anonymous strand as "sorted".
							-- If last anonymous strand reached, do not look for other strands with same name.
							if anon_strand_a = last_element (anonymous_strands) then
								null; -- strand_cursor already points the last strand in anonymous_strands --> do nothing
							else -- search for strands with same name
								strand_cursor_b := next (strand_cursor);
								while strand_cursor_b /= type_anonymous_strands.no_element loop
									
									anon_strand_b := element (strand_cursor_b); -- get anonymous strand

									if anon_strand_b.processed then

										if type_net_name."=" (anon_strand_b.name, anon_strand_a.name) then

											-- make sure scope of the strands are equal
											if anon_strand_a.scope /= anon_strand_b.scope then

												-- append segments to net
-- 												segment_cursor := b.segments.first; -- reset segment cursor to begin of segments of the current anonymous net
-- 												while segment_cursor /= type_anonymous_strand.no_element loop -- loop for each segment of anonymous_net "b"
-- 													s := element (segment_cursor);
-- 													type_net_segments.append (container => strand.segments, new_item => s);
-- 
-- 													if log_level >= 2 then
-- 														write_coordinates_of_segment (segment => s);
-- 													end if;
-- 													
-- 													next (segment_cursor);
-- 												end loop;
-- 
-- 												-- mark anonymous strand as "sorted" so that the outer loop can skip it in further spins
-- 												type_anonymous_strands.update_element (
-- 													container => anonymous_strands, 
-- 													position => strand_cursor_b,
-- 													process => set_sorted'access);

-- 											else
												log_indentation_reset;
												log (message_error & "contradicting scope of strands !"); -- CS: show strand name
												raise constraint_error;
											end if;
										end if;
									end if;

									next (strand_cursor_b); -- advance strand cursor
								end loop;
							end if;

							log_indentation_down;

                            -- assign coordinates
-- 							set_module (strand.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
							set_module (strand.coordinates, to_submodule_name (current_schematic));
                            set_path (strand.coordinates, path_to_submodule);
							set_sheet (strand.coordinates, sheet_number_current);

							-- set x,y coordinates (lowest available on the sheet)
							set (strand.coordinates, to_coordinates (lowest_xy (strand)));
							
							-- insert strand in module, then purge strand.segments for next spin
							log ("inserting strand in module ...", level => 3);
							add_strand (strand);
							type_net_segments.clear (strand.segments);

						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					log_indentation_down;
					
				else
					log (message_warning 
						 & "The schematic does not contain nets to associate net labels with !");
				end if;

				log_indentation_down;
			end associate_net_labels_with_anonymous_strands;
			
			procedure process_junctions is
			-- Breaks down all net segments where a junction sits on. In the end, the number of net segments increases.
				
			-- Loops in wild_segments and tests if a junction sits on a segment.
			-- Then splits the segment where the junction sits. If there are junctions left on the remaining fragments,
			-- they will be detected in the next spin. 
			-- The flag segment_smashed indicates there are no more segments left with a junction.
				segment : type_wild_net_segment;
				junction : type_net_junction;
				
				procedure change_segment_start_coordinates (segment : in out type_wild_net_segment) is 
				begin
					segment.coordinates_start := junction.coordinates;
				end change_segment_start_coordinates;
				
				segment_smashed : boolean := true; -- indicates whether a segment has been broken down

				use et_schematic.type_junctions;
				use type_wild_segments;
				
				junction_cursor : et_schematic.type_junctions.cursor; -- points to the junction being processed
				segment_cursor : type_wild_segments.cursor; -- points to the current segment

			begin -- process junctions
				
				log_indentation_up;
				
				-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
				-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
				if not is_empty (tmp_junctions) then 
					log ("processing" & count_type'image (length (tmp_junctions)) & " net junctions ...", log_threshold);

					-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
					-- does not increase anymore, all segments are processed.
					while segment_smashed loop
						
						segment_cursor := wild_segments.first;
						loop_s:
						--for s in 1..segment_count loop
						while segment_cursor /= type_wild_segments.no_element loop
						
							segment := type_wild_segments.element (segment_cursor); -- get a segment

							-- loop in junction list until a junction has been found that sits on the segment
							junction_cursor := tmp_junctions.first; -- reset junction cursor to begin of junction list
							while junction_cursor /= type_junctions.no_element loop

								-- fetch junction from current cursor position
								junction := type_junctions.element (junction_cursor);
								
								if junction_sits_on_segment (junction => junction, segment => type_net_segment (segment)) then -- match

									--write_coordinates_of_segment (type_net_segment(segment));
									--write_coordinates_of_junction (junction);
									if log_level >= log_threshold + 1 then
										log_indentation_up;
										log (to_string (position => junction.coordinates, scope => xy));
										log_indentation_down;
									end if;
									-- NOTE: junctions sitting on a net crossing may appear twice here.

									-- move start coord. of the current segment to the position of the junction
									type_wild_segments.update_element(
										container => wild_segments,
										position => segment_cursor,
										process => change_segment_start_coordinates'access
										);

									-- replace end coord. of segment by pos. of junction
									segment.coordinates_end := junction.coordinates;
									type_wild_segments.append(
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
						-- can be searched again. Otherwise clear segment. End of procedure.
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
			-- From the wild segment collection, assembles net segments to a list of anonymous strands.
			-- Takes junctions into account.

				procedure add_strand_to_anonymous_strands is
				-- Once an anonymous strand is complete, it gets appended to a list of anonymous strands. 
				-- Afterward the anonymous strand is deleted. It is a list of net segments which must be purged so that the list
				-- "anonymous_strand" can be filled with net segments of the next anonymous strand.
				begin
					type_anonymous_strands.append (anonymous_strands, anonymous_strand);
					type_anonymous_strand.clear (anonymous_strand.segments);
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

			



			function to_text return et_libraries.type_text is
			-- Converts a field like "F 1 "green" H 2700 2750 50  0000 C CNN" to a type_text
				function field (line : in type_fields_of_line; position : in positive) return string renames get_field_from_line;
		
				text_position : type_2d_point;
			begin
				set_x (text_position, mil_to_distance (field(line,5)));
				set_y (text_position, mil_to_distance (field(line,6)));
				return (
					-- read text field meaning
					meaning 	=> to_text_meaning(line => line, schematic => true),

					-- read content like "N701" or "NetChanger" from field position 3
					content		=> et_libraries.type_text_content.to_bounded_string (strip_quotes(field(line,3))),

					-- read orientation like "H"
					orientation	=> to_field_orientation (field(line,4)),

					-- read coordinates
					position	=> --(x => mil_to_distance (field(line,5)),
									  --y => mil_to_distance (field(line,6))),
									  text_position,
									  
					size		=> mil_to_distance (field (line,7)),
					style		=> to_text_style (style_in => field (line,10), text => false),
					line_width	=> et_libraries.type_text_line_width'first,

					-- build text visibility
					visible		=> to_field_visible (
										vis_in		=> field (line,8),
										schematic	=> true),

					-- build text alignment
					alignment	=> (
									horizontal	=> to_alignment_horizontal (field(line,9)),
									vertical	=> to_alignment_vertical   (field(line,10)))
					);
			end to_text;

		
	
			
			procedure insert_component is
			-- NOTE: This is schematic related !
			-- Inserts the component in the component list of the module (indicated by module_cursor).
			-- Components may occur multiple times, which implies they are
			-- split into units (EAGLE refers to them as "gates").
			-- Only the first occurence of the component leads to appending it to the component list of the module.
			
			-- The component to be inserted gets assembled with the temporarily variables assigned until now.
			-- Tests if a footprint has been associated with the component.

				-- This is required as scratch variable when breaking down the content of the footprint content.
				-- Kicad saves library and footprint name in a string like "bel_opto:LED_S_0805" separated by colon.
				-- When a real component (appearance sch_pcb) is inserted, this variable is loaded with
				-- the library name and the footprint.
				tmp_library_footprint : et_string_processing.type_fields_of_line;

				function field (
					line		: in et_string_processing.type_fields_of_line;
					position	: in positive) return string renames et_string_processing.get_field_from_line;

				use et_libraries;

			begin -- insert_component
				
				-- The compoenent is inserted into the components list of the module according to its appearance.
				-- If the component has already been inserted, it will not be inserted again.
				
				case tmp_component_appearance is
					
					when sch => -- we have a line like "L P3V3 #PWR07"
				
						et_schematic.add_component (
							reference => tmp_component_reference,
							component => (
								appearance		=> sch,

								-- Whether the component is a "power flag" can be reasoned from its reference:
								power_flag		=> to_power_flag (tmp_component_reference),
								
								name_in_library	=> tmp_component_name_in_lib,
								value 			=> et_libraries.type_component_value.to_bounded_string (et_libraries.content (tmp_component_text_value)),
								commissioned 	=> et_string_processing.type_date (et_libraries.content (tmp_component_text_commissioned)),
								updated 		=> et_string_processing.type_date (et_libraries.content (tmp_component_text_updated)),
								author 			=> et_libraries.type_person_name.to_bounded_string (et_libraries.content (tmp_component_text_author)),

								-- At this stage we do not know if and how many units there are. So the unit list is empty.
								units 			=> et_schematic.type_units.empty_map),
							log_threshold => log_threshold +1);

					when sch_pcb => -- we have a line like "L 74LS00 U1"

						-- break down the footprint content like "bel_opto:LED_S_0805".
						tmp_library_footprint := et_string_processing.read_line(
								line => et_libraries.content (tmp_component_text_packge),
								ifs => latin_1.colon);

						et_schematic.add_component ( 
							reference => tmp_component_reference,
							component => (
								appearance		=> sch_pcb,
								name_in_library	=> tmp_component_name_in_lib,
								value			=> et_libraries.type_component_value.to_bounded_string (et_libraries.content (tmp_component_text_value)),
								commissioned	=> et_string_processing.type_date (et_libraries.content (tmp_component_text_commissioned)),
								updated			=> et_string_processing.type_date (et_libraries.content (tmp_component_text_updated)),
								author			=> et_libraries.type_person_name.to_bounded_string (et_libraries.content (tmp_component_text_author)),

								-- properties of a real component (appears in schematic and layout);
								datasheet		=> et_libraries.type_component_datasheet.to_bounded_string (et_libraries.content (tmp_component_text_datasheet)),
								partcode		=> et_libraries.type_component_partcode.to_bounded_string (et_libraries.content (tmp_component_text_partcode)),
								purpose			=> et_libraries.type_component_purpose.to_bounded_string (et_libraries.content (tmp_component_text_purpose)),
								bom				=> et_libraries.type_bom'value (et_libraries.content (tmp_component_text_bom)),
								-- Assemble the package variant.
								-- NOTE: There is no way to identifiy the name of the package variant like TL084D or TL084N.
								-- For this reason we leave the variant name empty.
								variant =>
									( 
									variant => (

										-- get the package name from the footprint field 
										packge => 
											et_libraries.type_component_package_name.to_bounded_string(
												field(line => tmp_library_footprint, position => 2)),

										-- get the library file name from the footpint field
										library => et_libraries.type_full_library_name.to_bounded_string(
												field(line => tmp_library_footprint, position => 1))),

									-- The variant name is left empty.
									name => et_libraries.type_component_variant_name.to_bounded_string("")
									),

								-- At this stage we do not know if and how many units there are. So the unit list is empty for the moment.
								units => et_schematic.type_units.empty_map),

							log_threshold => log_threshold +1);

							
							-- Test if footprint has been associated with the component.
							if et_libraries.content (tmp_component_text_packge)'size = 0 then
								log_indentation_reset;
								log (
									text => message_error & "component " & et_schematic.to_string (tmp_component_reference) & " footprint not specified !",
									console => true);
								raise constraint_error;
							end if;

							-- The libaray and footpint name could be tested separately.
-- 							-- Test footprint contains a libaray name. example: "bel_opto:LED_S_0805"
-- 							if field(line => tmp_library_footprint, position => 1)'size = 0 then
-- 								write_message(
-- 									file_handle => current_output,
-- 									text => message_error & et_general.to_string(tmp_component_reference) & ": footprint library not specified !",
-- 									console => true);
-- 								raise constraint_error;
-- 							end if;
-- 
-- 							-- Test if footprint has been associated with the component.
-- 							if field(line => tmp_library_footprint, position => 2)'size = 0 then
-- 								write_message(
-- 									file_handle => current_output,
-- 									text => message_error & et_general.to_string(tmp_component_reference) & ": footprint not specified !",
-- 									console => true);
-- 								raise constraint_error;
-- 							end if;

					when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
						null;
						raise constraint_error;
						
				end case;


				exception
					when constraint_error =>
						log_indentation_reset;
						log (
							text => message_error & "component " & et_schematic.to_string (tmp_component_reference)
								& " " & et_coordinates.to_string (position => tmp_component_position),
							console => true);
						raise constraint_error;
				
			end insert_component;
			

			procedure insert_unit is 
			-- NOTE: This is schematic related.
			-- Inserts a unit into the unit list of a component. The text fields around a unit are placeholders.
			-- The properties of the placeholder texts are loaded with the properties of the text fields of the units
			-- found in the schematic. The idea behind is to store just basic text properties (type_text_basic) 
			-- for the texts around the unit, but not its content. The content is stored with the component as a kind
			-- of meta-data. See procedure insert_component.
			-- Raises constraint error if unit already in unit list of component.
			
				use et_libraries;
			
			begin -- insert_unit
				log_indentation_up;
				
				case tmp_component_appearance is

					when sch =>

						et_schematic.add_unit (
							reference	=> tmp_component_reference,
							unit_name	=> tmp_component_unit_name,
							unit 		=> (
								appearance		=> sch,
								position		=> tmp_component_position,
								orientation		=> tmp_component_unit_orientation,
								mirror			=> tmp_component_unit_mirror,
								name			=> tmp_component_unit_name,
								timestamp		=> tmp_component_timestamp,
								alt_repres		=> tmp_component_alt_repres,

								-- placeholders:
								-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
								reference		=> ( et_libraries.type_text_basic (tmp_component_text_reference)
													with meaning => tmp_component_text_reference.meaning ),
								value			=> ( et_libraries.type_text_basic (tmp_component_text_value)
													with meaning => tmp_component_text_value.meaning ),
								updated			=> ( et_libraries.type_text_basic (tmp_component_text_updated)
													with meaning => tmp_component_text_updated.meaning ),
								author			=> ( et_libraries.type_text_basic (tmp_component_text_author)
													with meaning => tmp_component_text_author.meaning ),
								commissioned	=>  ( et_libraries.type_text_basic (tmp_component_text_commissioned)
													with meaning => tmp_component_text_commissioned.meaning )
										   ),
							log_threshold => log_threshold + 1);
											   

					when sch_pcb =>

						et_schematic.add_unit (
							reference	=> tmp_component_reference,
							unit_name	=> tmp_component_unit_name,
							unit 		=> (
								appearance		=> sch_pcb,
								position		=> tmp_component_position,
								orientation		=> tmp_component_unit_orientation,
								mirror			=> tmp_component_unit_mirror,
								name			=> tmp_component_unit_name,
								timestamp		=> tmp_component_timestamp,
								alt_repres		=> tmp_component_alt_repres,

								-- placeholders:
								-- Convert tmp_component_text_* to a placeholder while maintaining the text meaning.
								reference		=> ( et_libraries.type_text_basic (tmp_component_text_reference)
													with meaning => tmp_component_text_reference.meaning ),
								value			=> ( et_libraries.type_text_basic (tmp_component_text_value)
													with meaning => tmp_component_text_value.meaning ),
								packge			=> ( et_libraries.type_text_basic (tmp_component_text_packge)
													with meaning => tmp_component_text_packge.meaning ),
								datasheet		=> ( et_libraries.type_text_basic (tmp_component_text_datasheet)
													with meaning => tmp_component_text_datasheet.meaning ),
								purpose			=> ( et_libraries.type_text_basic (tmp_component_text_purpose)
													with meaning => tmp_component_text_purpose.meaning ),
								partcode		=> ( et_libraries.type_text_basic (tmp_component_text_partcode)
													with meaning => tmp_component_text_partcode.meaning ),
								updated			=> ( et_libraries.type_text_basic (tmp_component_text_updated)
													with meaning => tmp_component_text_updated.meaning ),
								author			=> ( et_libraries.type_text_basic (tmp_component_text_author)
													with meaning => tmp_component_text_author.meaning ),
								commissioned	=>  (et_libraries.type_text_basic (tmp_component_text_commissioned)
													with meaning => tmp_component_text_commissioned.meaning),
								bom				=>  (et_libraries.type_text_basic (tmp_component_text_bom)
													with meaning => tmp_component_text_bom.meaning)
											),
							log_threshold => log_threshold + 1);

					when others => null; -- CS
				end case;

				log_indentation_down;
			end insert_unit;

	
			procedure verify_unit_name_and_position (line : in type_fields_of_line) is
			-- Checks if the x/y position of the unit matches that provided in given line.
			-- It is about the strange repetition of the unit name and its x/y coordinates in a line like
			-- "2    6000 4000"
				use et_libraries.type_unit_name;
				use et_coordinates;
			begin
				
				if et_libraries.to_string (tmp_component_unit_name) /= get_field_from_line (line,1) then
					raise constraint_error; -- CS: write useful message
				end if;
				
				if distance_x (tmp_component_position) /= mil_to_distance (get_field_from_line (line,2)) then
					log_indentation_reset;
-- 					log ("position invalid. expected '" & to_string (tmp_component_position.x) 
-- 						& "' found '" 
-- 						& get_field_from_line (line,2)
-- 						& "'");
					raise constraint_error; -- CS: write useful message
				end if;

				if distance_y (tmp_component_position) /= mil_to_distance (get_field_from_line (line,3)) then
					raise constraint_error; -- CS: write useful message
				end if;

			end verify_unit_name_and_position;

			procedure build_unit_orientation_and_mirror_style (line : in type_fields_of_line) is
			-- Builds from a line (see below) the component orientation and mirror style:

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
				orient_1 := type_schematic_unit_orientation'value (get_field_from_line (line, 1));
				orient_2 := type_schematic_unit_orientation'value (get_field_from_line (line, 2));
				mirror_1 := type_schematic_unit_mirror_style'value (get_field_from_line (line, 3));
				mirror_2 := type_schematic_unit_mirror_style'value (get_field_from_line (line, 4));

				case orient_1 is
					when -1 =>
						if orient_2 = 0 then
							tmp_component_unit_orientation := 180.0;

							-- compute unit mirror style
							if mirror_1 = 0 then
								case mirror_2 is
									when -1 =>
										tmp_component_unit_mirror := x_axis;
									when  1 =>
										tmp_component_unit_mirror := none;
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
								tmp_component_unit_orientation :=  90.0;
								
								-- compute unit mirror style
								case mirror_1 is
									when -1 =>
										if mirror_2 = 0 then
											tmp_component_unit_mirror := none;
										else
											-- invalid mirror style
											raise constraint_error;
										end if;

									when  0 =>
										-- invalid mirror style
										raise constraint_error;

									when  1 =>
										if mirror_2 = 0 then
											tmp_component_unit_mirror := x_axis;
										else
											-- invaid mirror style
											raise constraint_error;
										end if;
								end case;

							when  1 =>
								tmp_component_unit_orientation := -90.0;

								-- compute unit mirror style
								case mirror_1 is
									when -1 =>
										if mirror_2 = 0 then
											tmp_component_unit_mirror := x_axis;
										else
											-- invalid mirror style
											raise constraint_error;
										end if;

									when  0 =>
										-- invaid mirror style
										raise constraint_error;

									when  1 =>
										if mirror_2 = 0 then
											tmp_component_unit_mirror := none;
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
							tmp_component_unit_orientation := 0.0;

							-- compute unit mirror style
							if mirror_1 = 0 then
								case mirror_2 is
									when -1 =>
										tmp_component_unit_mirror := none;
									when  1 =>
										tmp_component_unit_mirror := x_axis;
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
			
			use et_coordinates;
			use et_libraries;
			
		begin -- read_schematic
			log_indentation_reset;
			log_indentation_up;
			
			if exists (to_string (current_schematic)) then
				log (text => "reading schematic file " & to_string (current_schematic) & " ...",
					 console => true);

				-- log module path as recorded by parent unit
				log_indentation_up;
				write_path_to_submodule;
				
				open (file => schematic_handle, mode => in_file, name => to_string (current_schematic));
				set_input (schematic_handle);
				while not end_of_file loop

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line(
								line => get_line,
								number => ada.text_io.line(current_input),
								comment_mark => "", -- there are no comment marks in the schematic file
								ifs => latin_1.space); -- fields are separated by space
					
					case field_count (line) is
						when 0 => null; -- we skip empty lines
						when others =>

							-- At a certain log level we report the whole line as it is:
 							log (to_string (line), level => log_threshold + 3);
							
							-- read schematic headline like "EESchema Schematic File Version 2"
							if not schematic_headline_processed then
								if get_field_from_line(line,1) = schematic_header_keyword_sys_name and
									get_field_from_line(line,2) = schematic_header_keyword_schematic and
									get_field_from_line(line,3) = schematic_header_keyword_file and
									get_field_from_line(line,4) = schematic_header_keyword_version then
										if positive'value(get_field_from_line(line,5)) = schematic_version then
											-- headline ok, version is supported
											schematic_headline_processed := true;

											-- Save schematic format version in sheet header:                                    
											sheet_header.version := positive'value(get_field_from_line(line,5));
										else
											log_indentation_reset;
											log (text => message_error & "schematic version" 
												 & positive'image(schematic_version) & " required.",
												console => true);
											raise constraint_error;
										end if;
								end if;
							else

								-- READ SHEET HEADER: stuff like:
								--     LIBS:nucleo_core-rescue
								--     LIBS:power
								--     LIBS:bel_connectors_and_jumpers
								--     LIBS:bel_primitives
								--     LIBS:bel_stm32
								--     LIBS:nucleo_core-cache
								--     EELAYER 25 0
								--     EELAYER END

								-- This data goes into a the sheet_header. When the schematic file has been
								-- read completely, the sheet_header is appended to global list_of_sheet_headers. 
								-- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.
								-- NOTE: The library entries in the header are not used by kicad. However, they must be read
								-- and stored in sheet_header.libraries.
								
								-- Field #1 of the line must be broken down by its own ifs in order to get "LIBS" and "bel_stm32"
								if get_field_from_line( get_field_from_line(line,1), 1, latin_1.colon) = schematic_library then

									-- for the log: write library name
									log (text => "uses library " & get_field_from_line (get_field_from_line (line,1), 2, latin_1.colon),
										 level => log_threshold + 1
										);

									-- Store bare library name in the list sheet_header.libraries:
									-- We use a doubly linked list because the order of the library names must be kept.
									et_libraries.type_library_names.append (
										container => sheet_header.libraries,
										new_item => et_libraries.type_library_name.to_bounded_string (
											get_field_from_line (get_field_from_line(line,1), 2, latin_1.colon))
										);

								end if;

								-- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
								-- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
								if get_field_from_line(line,1) = schematic_eelayer then
									if get_field_from_line(line,2) = schematic_eelayer_end then
										null;
									else
										-- append layer numbers to the sheet header
										sheet_header.eelayer_a := positive'value(
											get_field_from_line(line,2));

										sheet_header.eelayer_b := natural'value(
											get_field_from_line(line,3));
									end if;
								end if;
							
							
							
								-- READ DESCRIPTION:
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
								
								if not description_entered then
									if get_field_from_line(line,1) = schematic_description_header then -- $Descr A4 11693 8268
										description_entered := true; -- we are entering the sheet description

										-- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
										tmp_frame.paper_size	:= type_paper_size'value(get_field_from_line(line,2));
										tmp_frame.size_x		:= mil_to_distance (get_field_from_line(line,3));
										tmp_frame.size_y 		:= mil_to_distance (get_field_from_line(line,4)); 
										
										--tmp_frame.coordinates.path := path_to_submodule;
										set_path (tmp_frame.coordinates, path_to_submodule);
										--et_coordinates.set_module (tmp_frame.coordinates, et_coordinates.type_submodule_name.to_bounded_string (to_string (current_schematic)));
										set_module (tmp_frame.coordinates, to_submodule_name (current_schematic));

										-- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
										-- kicad built-in things and remain unassigned here.
																	
									end if;
								else -- we are inside the description
									if get_field_from_line(line,1) = schematic_description_footer then -- $EndDescr
										description_entered := false; -- we are leaving the description
										description_processed := true;

										-- Make temporarily tmp_title_block complete by assigning coordinates and list of texts.
										-- Then purge temporarily list of texts.
										-- Then append temporarily title block to main module.
										
										--tmp_title_block.coordinates.path := path_to_submodule;
										et_coordinates.set_path (tmp_title_block.coordinates, path_to_submodule);
										
										-- set_module (tmp_title_block.coordinates, et_coordinates.type_submodule_name.to_bounded_string (to_string(current_schematic)));
										set_module (tmp_title_block.coordinates, to_submodule_name (current_schematic));
										tmp_title_block.texts := tmp_title_block_texts; -- assign collected texts list to temporarily title block
										-- CS: x/y coordinates and list of lines are kicad built-in things and thus not available currently.

										-- purge temporarily texts
										type_title_block_texts.clear (tmp_title_block_texts);

										-- append title block to module
										add_title_block (tmp_title_block);
										
										-- append temporarily drawing frame to module
										add_frame (tmp_frame);
									end if;

									-- read endcoding from a line like "encoding utf-8"
									-- CS: checks only for a non-default endcoding and outputs a warning.
									-- CS: we assume only one encoding. other encodings are ignored currently.
									-- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
									-- good idea.
									if get_field_from_line(line,1) = schematic_keyword_encoding then
										if get_field_from_line(line,2) /= encoding_default then
											--log_indentation_reset;
											log (text => message_warning & "non-default endcoding '" & 
												get_field_from_line(line,2) & "' found !");
										end if;
									end if;
										
									-- read sheet number from a line like "Sheet 1 7"
									if get_field_from_line(line,1) = schematic_keyword_sheet then
										sheet_number_current := positive'value(get_field_from_line(line,2));
										log ("sheet" & positive'image (sheet_number_current) & " ...", level => log_threshold + 1);
										sheet_count_total    := positive'value(get_field_from_line(line,3));
										if sheet_count_total > 1 then
											-- Set in the list_of_submodules (to be returned) the parent_module. The schematic file 
											-- being processed (see input parameters of read_file_schematic_kicad) becomes the parent module
											-- of the submodules here.
											list_of_submodules.parent_module := et_coordinates.type_submodule_name.to_bounded_string (to_string (current_schematic));
										end if;
										-- CS: make sure total sheet count is less or equal current sheet number.

										-- Our temporarily drawing frame gets the current sheet number assigned.
										--tmp_frame.coordinates.sheet_number := sheet_number_current;
										et_coordinates.set_sheet (tmp_frame.coordinates, sheet_number_current);
									end if;						

									-- read sheet title from a line like "Title "abc""
									if get_field_from_line(line,1) = schematic_keyword_title then                        
										tmp_title_block_text.meaning := TITLE;
										tmp_title_block_text.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_title_block_texts.append (tmp_title_block_texts, tmp_title_block_text);
									end if;

									-- read date from a line like "Date "1981-01-23""
									if get_field_from_line(line,1) = schematic_keyword_date then                        
										tmp_title_block_text.meaning := DRAWN_DATE;
										tmp_title_block_text.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_title_block_texts.append (tmp_title_block_texts, tmp_title_block_text);
									end if;

									-- read revision from a line like "Rev "9.7.1"
									if get_field_from_line(line,1) = schematic_keyword_revision then                        
										tmp_title_block_text.meaning := REVISION;
										tmp_title_block_text.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_title_block_texts.append (tmp_title_block_texts, tmp_title_block_text);
									end if;

									-- read company name
									if get_field_from_line(line,1) = schematic_keyword_company then
										tmp_title_block_text.meaning := COMPANY;
										tmp_title_block_text.text := type_title_block_text_string.to_bounded_string(
											strip_quotes((get_field_from_line(line,2))));
										type_title_block_texts.append (tmp_title_block_texts, tmp_title_block_text);
									end if;

									-- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
									if  get_field_from_line(line,1) = schematic_keyword_comment_1 or
										get_field_from_line(line,1) = schematic_keyword_comment_2 or
										get_field_from_line(line,1) = schematic_keyword_comment_3 or 
										get_field_from_line(line,1) = schematic_keyword_comment_4 then
											tmp_title_block_text.meaning := MISC;
											tmp_title_block_text.text := type_title_block_text_string.to_bounded_string(
												strip_quotes((get_field_from_line(line,2))));
											type_title_block_texts.append (tmp_title_block_texts, tmp_title_block_text);
									end if;
									

								end if;

								-- Read submodule (sheet) sections (if there has been a total sheet count greater 1 detected earlier).
								-- NOTE: Such sections solely serve to display a hierarchical sheet as a black box with its ports.
								-- Rightly said this is the black box representation of a submodule. 
								-- So in the following we refer to them as "submodule".
								-- A submodule (sheet) section example:
								
								-- $Sheet
								-- S 4050 5750 1050 650 
								-- U 58A73B5D
								-- F0 "Sheet58A73B5C" 58
								-- F1 "morpho_test.sch" 58
								-- $EndSheet

								-- And add name of submodule (sheet file name) to list_of_submodules:
								if sheet_count_total > 1 then
									if not sheet_description_entered then
										if get_field_from_line(line,1) = schematic_sheet_header then -- $Sheet
											sheet_description_entered := true;
										end if;
									else -- we are inside a sheet description
										if get_field_from_line(line,1) = schematic_sheet_footer then -- $EndSheet
											sheet_description_entered := false; -- we are leaving the sheet description

											-- append name_of_submodule_scratch to list_of_submodules to be returned to parent unit
											type_submodule_names.append (list_of_submodules.list, name_of_submodule_scratch);

											-- append tmp_submodule_gui to list of gui submodules
											--type_gui_submodules.append (module.submodules, tmp_submodule_gui);
											add_gui_submodule (tmp_submodule_gui_name, tmp_submodule_gui);
										end if;

										-- read GUI submodule (sheet) position and size from a line like "S 4050 5750 1050 650"
										if get_field_from_line(line,1) = schematic_keyword_sheet_pos_and_size then
											set_path (tmp_submodule_gui.coordinates, path_to_submodule);
											--set_module (tmp_submodule_gui.coordinates, et_coordinates.type_submodule_name.to_bounded_string (to_string (current_schematic)));
											set_module (tmp_submodule_gui.coordinates, to_submodule_name (current_schematic));
											et_coordinates.set_sheet (tmp_submodule_gui.coordinates, sheet_number_current);
											
											et_coordinates.set_x (tmp_submodule_gui.coordinates, et_coordinates.mil_to_distance (get_field_from_line (line,2)));
											et_coordinates.set_y (tmp_submodule_gui.coordinates, et_coordinates.mil_to_distance (get_field_from_line (line,3)));

											tmp_submodule_gui.size_x := mil_to_distance (get_field_from_line(line,4));
											tmp_submodule_gui.size_y := mil_to_distance (get_field_from_line(line,5));                                
										end if;

										-- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
										if get_field_from_line(line,1) = schematic_keyword_sheet_timestamp then 
											tmp_submodule_gui.timestamp := type_timestamp (get_field_from_line(line,2));
										end if;
										
										-- Read submodule (sheet) name from a line like "F0 "mcu_stm32f030" 60"
										-- Since this is the black-box-representation of a kicad-sheet its name is threated as name of a submodule.
										-- The sheet name is stored in tmp_submodule_gui.name to be compared with the sheet file name later.
										if get_field_from_line(line,1) = schematic_keyword_sheet_name then
											tmp_submodule_gui_name := et_coordinates.type_submodule_name.to_bounded_string (strip_quotes (get_field_from_line(line,2)));
											--tmp_submodule_gui.text_size_of_name := type_text_size'value (get_field_from_line(line,3));
											tmp_submodule_gui.text_size_of_name := mil_to_distance (get_field_from_line(line,3));
										end if;

										-- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
										-- The file name (name_of_submodule_scratch) goes into the list of submodules to be returned to the parent unit.
										if get_field_from_line(line,1) = schematic_keyword_sheet_file then
											name_of_submodule_scratch := et_coordinates.type_submodule_name.to_bounded_string (strip_quotes (get_field_from_line (line,2)));
											--tmp_submodule_gui.text_size_of_file := et_libraries.type_text_size'value(get_field_from_line(line,3));
											tmp_submodule_gui.text_size_of_file := mil_to_distance (get_field_from_line (line,3));
											
											-- Test if sheet name and file name match:
											if et_coordinates.type_submodule_name.to_string (tmp_submodule_gui_name) /= base_name (et_coordinates.type_submodule_name.to_string (name_of_submodule_scratch)) then
												log (text => message_warning & "name mismatch: sheet: " &
													et_coordinates.type_submodule_name.to_string (tmp_submodule_gui_name) &
													" file: " & et_coordinates.type_submodule_name.to_string (name_of_submodule_scratch));
											end if;
										end if;

									end if;
								end if;

								-- Further parts of the file can be read IF the description has been processed before (see above)
								if description_processed then
									
									-- read net segments						
									if not net_segment_entered then
										-- collect net segments
										if get_field_from_line(line,1) = schematic_keyword_wire then
											if get_field_from_line(line,2) = schematic_keyword_wire then
												if get_field_from_line(line,3) = schematic_keyword_line then
													net_segment_entered := true; -- CS: assumption: segment coordinates follow in next line
-- 													log_indentation_up;
-- 													log (text => "net segment", level => 1);
-- 													log_indentation_down;
												end if;
											end if;
										end if;
									else
										net_segment_entered := false; -- we are leaving a net segment
			
										-- Build a temporarily net segment with fully specified coordinates:
										set_path (tmp_segment.coordinates_start, path_to_submodule);
										
										-- the name of the current submodule, which is in case of kicad the subordinated schematic file
										--set_module (tmp_segment.coordinates_start, type_submodule_name.to_bounded_string (to_string (current_schematic)));
										set_module (tmp_segment.coordinates_start, to_submodule_name (current_schematic));
										--set_module (tmp_segment.coordinates_end, type_submodule_name.to_bounded_string (to_string(current_schematic)));
										set_module (tmp_segment.coordinates_end, to_submodule_name (current_schematic));
										
										-- The sheet number. NOTE: Kicad V4 can handle only one sheet per submodule. The sheet numbering is consecutive and does
										-- not care about the actual submodule names.
										set_sheet (tmp_segment.coordinates_start, sheet_number_current);
										set_sheet (tmp_segment.coordinates_end,   sheet_number_current);

										-- the x/y position
										set_x (tmp_segment.coordinates_start, mil_to_distance (get_field_from_line (line,1)));
										set_y (tmp_segment.coordinates_start, mil_to_distance (get_field_from_line (line,2)));
										set_x (tmp_segment.coordinates_end, mil_to_distance (get_field_from_line (line,3)));
										set_y (tmp_segment.coordinates_end, mil_to_distance (get_field_from_line (line,4)));

										-- Ignore net segments with zero length (CS: for some reason they may exist. could be a kicad bug)
										-- If a net segment has zero length, issue a warning.
										if length (tmp_segment) > zero_distance then 

											-- The net segments are to be collected in a wild list of segments for later sorting.
											if log_level >= log_threshold + 1 then
												log_indentation_up;
												--write_coordinates_of_segment (tmp_segment);
												log ("net segment " & to_string (segment => tmp_segment, scope => xy));
												log_indentation_down;
											end if;
											
											type_wild_segments.append (wild_segments, tmp_segment);
											
										else -- segment has zero length
											log (message_warning & affected_line (line) & "Net segment with zero length found -> ignored !");
										end if; -- length

									end if;

									-- read net junctions and store them in a wild list of net junctions for later sorting
									if get_field_from_line(line,1) = schematic_keyword_connection then
										if get_field_from_line(line,2) = schematic_tilde then

											-- build a temporarily junction
											set_path (tmp_junction.coordinates, path_to_submodule);
											--set_module (tmp_junction.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
											set_module (tmp_junction.coordinates, to_submodule_name (current_schematic));
											set_sheet (tmp_junction.coordinates, sheet_number_current);
											set_x (tmp_junction.coordinates, mil_to_distance (get_field_from_line (line,3)));
											set_y (tmp_junction.coordinates, mil_to_distance (get_field_from_line (line,4)));

											-- for the log
											--write_junction_properties (tmp_junction);
											if log_level >= log_threshold + 1 then
												log_indentation_up;
												log ("junction at " & to_string (junction => tmp_junction, scope => xy));
												log_indentation_down;
											end if;

											type_junctions.append (tmp_junctions, tmp_junction);
										end if;
									end if;
										
									-- Read simple net labels (they do not have a tag, but just a text) 
									-- CS: assumption: keywords "Text Label" and coordinates in one line
									if not simple_label_entered then							
										if 	get_field_from_line(line,1) = schematic_keyword_text and 
											get_field_from_line(line,2) = schematic_keyword_label_simple then

											simple_label_entered := true;

											-- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
											set_path (tmp_simple_net_label.coordinates, path_to_submodule);
											--set_module (tmp_simple_net_label.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
											set_module (tmp_simple_net_label.coordinates, to_submodule_name (current_schematic));
											set_sheet (tmp_simple_net_label.coordinates, sheet_number_current);
											set_x (tmp_simple_net_label.coordinates, mil_to_distance (get_field_from_line (line,3)));
											set_y (tmp_simple_net_label.coordinates, mil_to_distance (get_field_from_line (line,4)));
											tmp_simple_net_label.orientation := to_angle (get_field_from_line (line,5));

											tmp_simple_net_label.size := mil_to_distance (get_field_from_line(line,6));
											-- CS: check label text size 1.27
											
											tmp_simple_net_label.style := to_text_style (style_in => get_field_from_line(line,7), text => true);
											-- cS: check label style
											
											tmp_simple_net_label.width := et_libraries.type_text_line_width'value(get_field_from_line(line,8));
											-- CS: check label line width

										end if;
									else
										simple_label_entered := false; -- we are leaving a simple label

										-- get label text and put it to temporarily simple label
										tmp_simple_net_label.text := type_net_name.to_bounded_string(get_field_from_line(line,1));

										-- for the log
										--write_label_properties (type_net_label (tmp_simple_net_label));
										if log_level >= log_threshold + 1 then
											log_indentation_up;
											log ("simple label at " & to_string (label => type_net_label (tmp_simple_net_label), scope => xy));
											log_indentation_down;
										end if;
										
										-- The simple labels are to be collected in a wild list of simple labels.
										type_simple_labels.append (tmp_wild_simple_labels,tmp_simple_net_label);
									end if;
									
									-- read tag net labels (tagged labels can be global or hierarchical)
									if not tag_label_entered then
										if 	get_field_from_line(line,1) = schematic_keyword_text 
											and 
											(get_field_from_line(line,2) = schematic_keyword_label_hierarchic 
											or get_field_from_line(line,2) = schematic_keyword_label_global)
											then
										
											tag_label_entered := true;

											-- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
											-- The keyword in field 2 tells whether we have a hierarchic or global label:
											if get_field_from_line(line,2) = schematic_keyword_label_hierarchic then
												tmp_tag_net_label.hierarchic := true;
												tmp_tag_net_label.global := false;
											else
												tmp_tag_net_label.hierarchic := false;
												tmp_tag_net_label.global := true;
											end if;

											set_path (tmp_tag_net_label.coordinates, path_to_submodule);
											--set_module (tmp_tag_net_label.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
											set_module (tmp_tag_net_label.coordinates, to_submodule_name (current_schematic));
											set_sheet (tmp_tag_net_label.coordinates, sheet_number_current);
											set_x (tmp_tag_net_label.coordinates, mil_to_distance (get_field_from_line (line,3)));
											set_y (tmp_tag_net_label.coordinates, mil_to_distance (get_field_from_line (line,4)));
											tmp_tag_net_label.orientation   := to_angle (get_field_from_line(line,5));
											
											tmp_tag_net_label.direction := to_direction(
												get_field_from_line(line,7)
												);

											-- build text attributes from size, font and line width
											tmp_tag_net_label.size := mil_to_distance (get_field_from_line(line,6));
											tmp_tag_net_label.style := to_text_style (style_in => get_field_from_line(line,8), text => true);
											tmp_tag_net_label.width := et_libraries.type_text_line_width'value(get_field_from_line(line,9));
										end if;
									else
										tag_label_entered := false; -- we are leaving a tag label

										-- get label text and put it to temporarily tag label
										tmp_tag_net_label.text := type_net_name.to_bounded_string(get_field_from_line(line,1));

										-- for the log
										if log_level >= log_threshold + 1 then
											log_indentation_up;
											log ("tag label at " & to_string (label => type_net_label (tmp_tag_net_label), scope => xy));
											log_indentation_down;
										end if;
										
										-- The tag labels are to be collected in a wild list of tag labels for later sorting.
										type_tag_labels.append (tmp_wild_tag_lables,tmp_tag_net_label);
									end if;

									-- read note from a line like "Text Notes 3400 2800 0 60 Italic 12" followed by a line with the actual note:
									if not note_entered then
										if 	get_field_from_line(line,1) = schematic_keyword_text and 
											get_field_from_line(line,2) = schematic_keyword_note then
												note_entered := true; -- we are entering a note
										
												-- set coordinates
												set_path (tmp_note.coordinates, path_to_submodule);
												--set_module (tmp_note.coordinates, type_submodule_name.to_bounded_string (to_string (current_schematic)));
												set_module (tmp_note.coordinates, to_submodule_name (current_schematic));
												set_sheet (tmp_note.coordinates, sheet_number_current);
												set_x (tmp_note.coordinates, mil_to_distance (get_field_from_line (line,3)));
												set_y (tmp_note.coordinates, mil_to_distance (get_field_from_line (line,4)));
												tmp_note.orientation   := to_angle (get_field_from_line(line,5));
												tmp_note.size := mil_to_distance (get_field_from_line(line,6));
												tmp_note.style := to_text_style (style_in => get_field_from_line(line,7), text => true);
												tmp_note.line_width := mil_to_distance (get_field_from_line (line,8));

										end if;
									else 
										note_entered := false; -- we are leaving a note

										-- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
										-- CS: store lines in a list of lines instead ?
										-- CS: Currently we store the line as it is in tmp_note.text
										tmp_note.content := et_libraries.type_text_content.to_bounded_string(to_string(line));

										write_note_properties (tmp_note, log_threshold + 1);
										
										-- the notes are to be collected in the list of notes
										add_note (tmp_note);
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
										if get_field_from_line(line,1) = schematic_component_header then
											component_entered := true;

											-- This is to init the temporarily used variables that store text fields.
											-- This clears the "field found" flags
											init_temp_variables;

										end if;
									else -- we are inside the component and wait for the component footer ($EndComp)
										if get_field_from_line(line,1) = schematic_component_footer then
											component_entered := false; -- we are leaving the component

											-- Check if all required text fields have been found.
											-- Check content of text fields for syntax and plausibility.
											check_text_fields (log_threshold + 1);
											
											-- Insert component in component list of module. If a component is split
											-- in units, only the first occurence of it leads to inserting the component.
											-- Nevertheless there are some checks on the unit (see insert_component).
											insert_component;
	
											-- We update the component with the collected unit information.
											insert_unit;

										else
											-- READ COMPONENT SECTION CONTENT
											
											-- Read component name and annotation from a line like "L NetChanger N1". 
											-- From this entry we reason the compoenent appearance.
											if get_field_from_line(line,1) = schematic_component_identifier_name then -- "L"
												
												tmp_component_name_in_lib := et_libraries.type_component_name.to_bounded_string(get_field_from_line(line,2)); -- "SN74LS00"
												tmp_component_appearance := to_appearance(line => line, schematic => true);
												
												case tmp_component_appearance is
												
													when et_libraries.sch => 
														-- we have a line like "L P3V3 #PWR07"
														tmp_component_reference := et_schematic.to_component_reference(
																text_in => get_field_from_line(line,3),
																allow_special_character_in_prefix => true);

													when et_libraries.sch_pcb =>

														-- we have a line like "L 74LS00 U1"
														tmp_component_reference := et_schematic.to_component_reference(
																text_in => get_field_from_line(line,3),
																allow_special_character_in_prefix => false);

													when others => -- CS: This should never happen. A subtype of type_component_appearance could be a solution.
														null;
														raise constraint_error;
														
												end case;
															
												-- CS: check proper annotation

											-- read line like "U 2 1 4543D4D3F" 
											-- U is the line indicator, 2 is the unit id, 1 is the demorgan flag, last field is the timestamp
											elsif get_field_from_line(line,1) = schematic_component_identifier_unit then -- "U"

												-- KiCad uses positive numbers to identifiy units. But in general a unit name can
												-- be a string as well. Therefore we handle the unit id as string.
												tmp_component_unit_name := et_libraries.type_unit_name.to_bounded_string(
													get_field_from_line(line,2)); -- the unit id

												-- Read DeMorgan flag:
												tmp_component_alt_repres := to_alternative_representation(line => line, schematic => true);

												-- Read and check the timestamp:
												tmp_component_timestamp := type_timestamp(get_field_from_line(line,4));
												et_string_processing.check_timestamp (tmp_component_timestamp);

											-- Read unit coordinates from a line like "P 3200 4500".
											elsif get_field_from_line(line,1) = schematic_component_identifier_coord then -- "P"
											
												set_x (tmp_component_position, mil_to_distance (get_field_from_line (line,2))); -- "3200"
												set_y (tmp_component_position, mil_to_distance (get_field_from_line (line,3))); -- "4500"

												-- The unit coordinates is more than just x/y :
												-- unit_scratch.coordinates.main_module := module.name;
												set_path (tmp_component_position, path_to_submodule);
												--set_module (tmp_component_position, type_submodule_name.to_bounded_string (to_string (current_schematic)));
												set_module (tmp_component_position, to_submodule_name (current_schematic));
												set_sheet (tmp_component_position, sheet_number_current);

											-- Skip unit path entry in lines like "AR Path="/59EF082F" Ref="N23"  Part="1"
											elsif get_field_from_line (line,1) = schematic_component_identifier_path then -- "AR"
												-- CS: meaning unclear
												log (message_warning & affected_line (line) & "ignoring line '" & to_string (line) & "' ! Meaning unclear !");

											-- read unit fields 0..2 from lines like:
											-- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
											--			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
											--			"F 2 "bel_netchanger:N_0.2MM" H 2600 2100 60  0001 C CNN"
											--
											-- set "field found" flags
											elsif get_field_from_line(line,1) = component_field_identifier then -- "F"
												
												case type_component_field_id'value (get_field_from_line (line,2)) is
													when component_field_reference =>
														tmp_component_text_reference_found	:= true;
														tmp_component_text_reference 		:= to_text;

													when component_field_value =>
														tmp_component_text_value_found		:= true;
														tmp_component_text_value 			:= to_text;
														
													when component_field_footprint =>
														tmp_component_text_packge_found		:= true;
														tmp_component_text_packge 			:= to_text;
														
													when component_field_datasheet =>
														tmp_component_text_datasheet_found	:= true;
														tmp_component_text_datasheet 		:= to_text;
														
													when component_field_function =>
														tmp_component_text_purpose_found	:= true;
														tmp_component_text_purpose 			:= to_text;
														
													when component_field_partcode =>
														tmp_component_text_partcode_found	:= true;
														tmp_component_text_partcode 		:= to_text;
														
													when component_field_commissioned =>
														tmp_component_text_commissioned_found	:= true;
														tmp_component_text_commissioned 		:= to_text;
														
													when component_field_updated =>
														tmp_component_text_updated_found	:= true;
														tmp_component_text_updated 			:= to_text;
														
													when component_field_author =>
														tmp_component_text_author_found		:= true;
														tmp_component_text_author 			:= to_text;

													when component_field_bom =>
														tmp_component_text_bom_found		:= true;
														tmp_component_text_bom				:= to_text;

														
													when others => null; -- CS: other fields are ignored. warning ?
												end case;

											else
												-- What is left is a strange repetition of the unit name and its x/y coordinates in a line like
												-- "2    6000 4000"
												-- followed by the unit mirror style and the unit orientation in a line like
												-- "1    0    0    -1"

												case field_count (line) is
													when 3 => -- we have the unit name and its x/y position.
														-- We verify if unit name and position match the values read earlier:
														verify_unit_name_and_position (line);
													
													when 4 => null; -- we have the unit mirror style and orientation
														build_unit_orientation_and_mirror_style (line);
													
													when others => 
														raise constraint_error; -- CS: write useful message
												end case;

											end if;
									end if;
									
								end if;
							end if;

							end if; -- if not schematic_header_processed

					end case;

				end loop;

                -- If file has been read and no header found:
				if not schematic_headline_processed then
					log_indentation_reset;
                    log (
						text => message_error & "Schematic file header invalid or not found ! File not accepted !",
						console => true);
                    raise constraint_error;
                end if;

                -- Add sheet_header to module.
                -- NOTE: The file name serves as key in order to match from file to header.
				add_sheet_header (
					header => sheet_header,
					sheet => current_schematic);
				
				close (schematic_handle);
				log_indentation_down;
				log ("reading complete. closing schematic file " & to_string (current_schematic) & " ...", log_threshold);

				-- From the wild list of net segments, assembles net segments to a list of anonymous strands.
				build_anonymous_strands; 
	
				-- All anonymous strands must be given a name. The name is enforced by the a net label. The first label found on the strand
				-- sets the strand name. 
				-- Other labels on the strand are checked for their name only. If the name differs from the net name set earlier,
				-- a warning is output.
				-- Strads without label remain anonymous by using the notation "N$"
				-- The nets are finally appended to the strands of the current module.
				associate_net_labels_with_anonymous_strands;

			else
				log_indentation_reset;
				log (message_error & "schematic file '" & to_string (current_schematic) & "' not found !",
					console => true);
				raise constraint_error;
			end if;

			return list_of_submodules;

			exception
				-- CS: log exception message
				when event:
					constraint_error =>
						log_indentation_reset;
						log (message_error & "in schematic file '" 
							& to_string (current_schematic) & "' " 
							& et_string_processing.affected_line (line),
							console => true);
							et_import.close_report;
-- 						put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
						raise;

				when others =>
					log_indentation_reset;
					log (message_error & "in schematic file '" 
						 & to_string (current_schematic) & "' " 
						 & et_string_processing.affected_line (line),
						console => true);
					et_import.close_report;
-- 					put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;					

		end read_schematic;


	begin -- import_design

		-- change to given project directory
		log (
			text => "changing to project directory '" & (type_project_name.to_string (et_schematic.project_name) & "' ..."),
			level => log_threshold
			);
		set_directory (type_project_name.to_string (et_schematic.project_name));
		
		case et_import.cad_format is
			when et_import.kicad_v4 =>

				-- Kicad uses Y axis positive downwards style
				Y_axis_positive := downwards;
				
				-- derive top level schematic file name from project name
				top_level_schematic := read_project_file;
				tmp_module_name := type_submodule_name.to_bounded_string (base_name (to_string (top_level_schematic)));
				
				-- The top level schematic file dictates the module name. So we create the module here.
				-- The first element to set is the project libraries which we collected earlier when the
				-- project file was read.
				add_module (
					module_name	=> tmp_module_name,
					module		=> (
						libraries		=> tmp_project_libraries, -- set project libraries
						strands			=> type_strands_named.empty_list,
						nets			=> type_nets.empty_map,
						components		=> type_components.empty_map,
						submodules		=> type_gui_submodules.empty_map,
						frames			=> type_frames.empty_list,
						title_blocks	=> type_title_blocks.empty_list,
						notes			=> type_texts.empty_list,
						sheet_headers	=> type_sheet_headers.empty_map
						)
					);
				
				read_components_libraries (log_threshold); -- as stored in element "libraries" of the current module
				current_schematic := top_level_schematic;

                -- The top level schematic file is the first entry in the module path.
				append_name_of_parent_module_to_path (tmp_module_name);
                
				-- Starting from the top level module, we read its schematic file. The result can be a list of submodules.
				-- NOTE: Kicad refers to them as "sheets" !
				
				-- The function read_schematic requires the name of the current submodule,
				-- It returns a list of submodules.
				list_of_submodules := read_schematic (current_schematic, log_threshold);

				log("DESIGN STRUCTURE ");
				log_indentation_up;
				
				-- If read_file_schematic_kicad returns an empty list of submodules, we are dealing with a flat design. Otherwise
				-- the design is hierarchic (because the submodule list is longer than zero).
				--if type_submodule_names.length (list_of_submodules.list) = 0 then -- flat design -- CS: use is_empty
				if type_submodule_names.is_empty (list_of_submodules.list) then -- flat design
					log ("FLAT");
				else -- hierarchic design
					-- In the follwing we dive into the submodules. Each time before a deeper level is entered,
					-- the list of submodules of the current level is saved on a LIFO stack.
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
						-- fetch name of submodule (where id is pointing at)
						current_schematic := type_schematic_file_name.to_bounded_string (
							et_coordinates.type_submodule_name.to_string (
								type_submodule_names.element (
									container => list_of_submodules.list,
									index => list_of_submodules.id)));
						
						-- backup list_of_submodules OF THIS LEVEL on stack (including the current submodule id)
						push (list_of_submodules);
						log ("DESCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
						log (row_separator_single);
						
						-- Read schematic file as indicated by list_of_submodules.id. 
						-- Read_schematic receives the name of the schematic file to be read.
						list_of_submodules := read_schematic (current_schematic, log_threshold);

						-- If the schematic file contains submodules (hierarchic sheets), set list_of_submodules.id to the first 
						-- submodule of them. Otherwise restore submodule list of parent module and advance therein to next submodule.
						if type_submodule_names.length (list_of_submodules.list) = 0 then -- flat submodule (no hierarchic sheets) -- CS: use is_empty

							list_of_submodules := pop;
                            list_of_submodules.id := list_of_submodules.id + 1;
                            --delete_last_module_name_from_path;
							log ("NO SUBMODULES HERE. ASCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
							log (row_separator_single);

						else
							-- set cursor at first submodule of list and append name of parent module to path_to_submodule
                            list_of_submodules.id := 1;
                            append_name_of_parent_module_to_path (list_of_submodules.parent_module);
						end if;

						-- Once the last submodule of the list has been processed, restore list of the overlying level and advance to next module.
						-- Exit after last submodule in level 0 has been processed.
						if list_of_submodules.id > positive (type_submodule_names.length (list_of_submodules.list)) then
							if depth = 0 then 
								log ("LAST SUBMODULE PROCESSED.");
								exit; 
							end if;
							list_of_submodules := pop; -- restore overlying list
                            list_of_submodules.id := list_of_submodules.id + 1;
                            delete_last_module_name_from_path; -- update path_to_submodule
							log ("LAST SUBMODULE PROCESSED. ASCENDING TO HIERARCHY LEVEL: -" & trim(natural'image(depth),left));
							log (row_separator_single);
						end if;
						
					end loop;

					log_indentation_down;

					-- Checks scope of strands across the current module (indicated by module_cursor).
					-- NOTE: module_cursor points to the current module.
					--check_strands; -- CS: currently not used
					
				end if;

				log_indentation_down;
				
			when others =>
				null; -- CS: add import of other CAD formats here

				
		end case;

		-- CS: exception handler
		
	end import_design;

end et_kicad;

-- Soli Deo Gloria
