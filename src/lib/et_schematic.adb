------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET SCHEMATIC DECLARATIONS                      --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_geometry;
with et_export;
with et_csv;
with et_netlist;

package body et_schematic is

	function to_string (schematic : in type_schematic_file_name.bounded_string) return string is
	-- Returns the given schematic file name as string.
	begin
		return type_schematic_file_name.to_string (schematic);
	end to_string;
	
	-- Sometimes we need to output the location of a submodule:
	procedure write_path_to_submodule is
		use et_coordinates;
		c : type_path_to_submodule.cursor;
		use et_string_processing;
	begin
		log (text => "path/location");
		log_indentation_up;
		
		c := type_path_to_submodule.first (path_to_submodule);            

		-- If there is a hierarchy deeper than 1, write path to submodule:
		if type_path_to_submodule.length(path_to_submodule) > 1 then
			for n in 1..type_path_to_submodule.length(path_to_submodule)-1 loop
				log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
				c := type_path_to_submodule.next(c);
			end loop;
		
			c := type_path_to_submodule.last(path_to_submodule);

			-- write the submodule name
			log_indentation_up;
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
			log_indentation_down;
		else
			-- no hierarchy. write just the submodule name
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
		end if;
		
		log_indentation_down;
	end write_path_to_submodule;

	-- Here we append a submodule name the the path_to_submodule.
	procedure append_name_of_parent_module_to_path (submodule : in et_coordinates.type_submodule_name.bounded_string) is
		use et_string_processing;
		use ada.directories;
		use et_coordinates;
	begin
		log (text => "path_to_submodule: appending submodule " & type_submodule_name.to_string(submodule), level => 2);
		-- Since we are dealing with file names, the extension must be removed before appending.
		type_path_to_submodule.append (path_to_submodule,
			type_submodule_name.to_bounded_string (base_name (type_submodule_name.to_string(submodule)))
			);
	end append_name_of_parent_module_to_path;
	
	-- Here we remove the last submodule name form the path_to_submodule.
	procedure delete_last_module_name_from_path is
		use et_coordinates;
	begin
		type_path_to_submodule.delete_last (path_to_submodule);
	end delete_last_module_name_from_path;

	function to_submodule_name (file_name : in type_schematic_file_name.bounded_string)
		return et_coordinates.type_submodule_name.bounded_string is
	-- Retruns the base name of the given schematic file name as submodule name.
		use ada.directories;
	begin
		-- CS: test if given submodule has an extension. if not return
		-- submodule as it is.
		--return to_bounded_string (base_name (et_coordinates.to_string (submodule)));
		return type_submodule_name.to_bounded_string (base_name (to_string (file_name)));
	end to_submodule_name;
	
	function to_string (net_name : in type_net_name.bounded_string) return string is
	-- Returns the given net name as string.
	begin
		return type_net_name.to_string (net_name);
	end to_string;

	function simple_name (net_name : in type_net_name.bounded_string) return type_net_name.bounded_string is
	-- Returns the simple name of the given net name.
	-- Example: If the given name is "MOTOR_DRIVER.CLOCK" then the return is "CLOCK".
		position_of_last_separator : natural := 0;
		name : type_net_name.bounded_string;
	begin
		-- Detect position of last hierarchy separator.
		position_of_last_separator := index (net_name, hierarchy_separator, backward);

		-- If the given net name is a simple name already, return it as it is.
		-- Otherwise extract the simple net name from position of hierarchy 
		-- separator until end of net_name.
		if position_of_last_separator > 0 then
			name := type_net_name.to_bounded_string (
				slice (net_name, position_of_last_separator + 1, length (net_name)));
		else
			name := net_name;
		end if;
		
		return name;
	end simple_name;
	
	function anonymous (net_name : in type_net_name.bounded_string) return boolean is
	-- Returns true if the given net name is anonymous.
	begin
		-- CS: this is just a test if anonymous_net_name_prefix is somewhere
		-- in the net name. Should be improved.
		if type_net_name.count (net_name, anonymous_net_name_prefix) > 0 then
			return true;
		else 
			return false;
		end if;
	end anonymous;
	
	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_coordinates;

		log_threshold : type_log_level := 2;
	begin
		log_indentation_up;
		
		case label.label_appearance is
			when simple =>
				log (text => "simple label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				
			when tag =>
				if label.hierarchic then
					log (text => "hierarchic label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				end if;
				if label.global then
					log (text => "global label " & to_string (label.text) & " at " & to_string (position => label.coordinates));
				end if;
					-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log (text => to_string (label.orientation), level => log_threshold + 1);
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_down;
		log_indentation_down;

	end write_label_properties;

	function to_string (label : in type_net_label; scope : in type_scope) return string is
	-- Returns the coordinates of the given label as string.
	begin
		return (to_string (position => label.coordinates, scope => scope));
	end to_string;
	
	procedure write_junction_properties (junction : in type_net_junction) is
	-- Writes the properties of the given net junction in the logfile.
		use et_string_processing;
		use et_coordinates;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("net junction");
		log_indentation_up;
		log (to_string (position => junction.coordinates), log_threshold);
		log_indentation_down;
		log_indentation_down;		
	end write_junction_properties;

	function to_string (junction : in type_net_junction; scope : in type_scope) return string is
	-- Returns the position of the given junction as string.
	begin	
		return (to_string (position => junction.coordinates, scope => scope));
	end to_string;
	
	procedure write_note_properties (
		note : in et_schematic.type_note;
		log_threshold : in et_string_processing.type_log_level := 0) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_coordinates;
	
-- 		log_threshold : type_log_level := 2;
	begin
		log_indentation_up;
		log ("text note", log_threshold);

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log (text => "content '" & type_text_content.to_string (note.content) & "'", level => log_threshold);
		else
			log (text => et_string_processing.message_warning & "no content !", level => log_threshold); 
		end if;

		
		if log_level >= log_threshold + 1 then
			
			-- position
			log (to_string (position => note.coordinates));
			
			-- size
			log ("size" & et_libraries.type_text_size'image (note.size));

			-- style
			log ("style " & to_lower(et_libraries.type_text_style'image (note.style)));

			-- line width
			log ("line width" & et_libraries.type_text_line_width'image (note.line_width));

			-- angle
			log (to_string (note.orientation));

			-- visible
			log ("visible " & to_lower (et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log ("alignment (hor/vert) "
				& to_lower (et_libraries.type_text_alignment_horizontal'image (note.alignment.horizontal))
				& "/"
				& to_lower (et_libraries.type_text_alignment_vertical'image (note.alignment.vertical)));

		end if;
		
		log_indentation_down;
		log_indentation_down;
	end write_note_properties;
	
	procedure write_component_properties (
	-- Writes the properties of the component indicated by the given cursor.
		component : in type_components.cursor;
		log_threshold : in et_string_processing.type_log_level) is

		use et_string_processing;
	begin
		log_indentation_up;
		
		-- reference (serves as key in list of components)
		log ("component " & to_string (type_components.key (component)) & " properties", log_threshold);

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log ("name in library "
			& to_string (type_components.element (component).name_in_library), log_threshold);
		
		-- value
		log ("value "
			& to_string (type_components.element (component).value), log_threshold);

		-- commissioned
		log ("commissioned "
			& string (type_components.element (component).commissioned), log_threshold);

		-- updated
		log ("updated      "
			& string (type_components.element(component).updated), log_threshold);

		-- author
		log ("author "
			& to_string (type_components.element(component).author), log_threshold);
		
		-- appearance
		log (to_string (type_components.element(component).appearance), log_threshold);

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				log (to_string (type_components.element (component).variant.variant), log_threshold);
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				log ("datasheet "
					& type_component_datasheet.to_string (type_components.element (component).datasheet), log_threshold);

				-- partcode
				log ("partcode "
					& type_component_partcode.to_string (type_components.element (component).partcode), log_threshold);
				
				-- purpose
				log ("purpose "
					& type_component_purpose.to_string (type_components.element(component).purpose), log_threshold);

				-- bom
				log ("bom "
					& to_string (type_components.element (component).bom), log_threshold);

				
			when pcb => null; -- CS
			when others => null; -- CS should never happen as virtual components do not have a package
		end case;

		log_indentation_down;
		log_indentation_down;
		
	end write_component_properties;

	function component_reference (cursor : in type_components.cursor) 
		return type_component_reference is
	-- Returns the component reference where cursor points to.
	begin
		return type_components.key (cursor);
	end component_reference;

	function component_appearance (cursor : in type_components.cursor)
	-- Returns the component appearance where cursor points to.
		return type_component_appearance is
	begin
		return type_components.element (cursor).appearance;
	end component_appearance;

	function bom (cursor : in type_components.cursor)
	-- Returns the component bom status where cursor points to.
		return type_bom is
		b : type_bom; -- the bom status
	begin
		-- Only real components have a bom status.
		if component_appearance (cursor) = sch_pcb then
			b := type_components.element (cursor).bom;
		end if;
		return b;
	end bom;
	
	function component_power_flag (cursor : in type_components.cursor)
	-- Returns true if the component is a power flag.
		return boolean is
		use et_string_processing;
	begin
		-- Only vitual components have the power flag property. 
		-- For real components the return is always false;
		if type_components.element (cursor).appearance = sch then
			--log ("virtual component");
			--if type_components.element (cursor).power_flag then
			--	log ("power flag on");
			--else
			--	log ("power flag off");
			--end if;
			return type_components.element (cursor).power_flag;
		else
			--log ("real component");
			return false;
		end if;
	end component_power_flag;
	
	function component_name_in_library (cursor : in type_components.cursor) 
		return et_libraries.type_component_name.bounded_string is
	-- Returns the generic name of a component as it is listed in a library.
	-- The cursor must point to the component in question.
	begin
		return type_components.element (cursor).name_in_library;
	end component_name_in_library;


	function unit_exists (
	-- Returns true if the unit with the given name exists in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return boolean is

		use et_schematic;
		use et_schematic.type_units;
	begin
		if et_schematic.type_units.find (container => units, key => name) = type_units.no_element then
			return false;
		else	
			return true;
		end if;
	end unit_exists;
	
	function position_of_unit (
	-- Returns the coordinates of the unit with the given name.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return et_coordinates.type_coordinates is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).position;
	end position_of_unit;

	function mirror_style_of_unit (
	-- Returns the mirror style of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return et_schematic.type_mirror is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).mirror;
	end mirror_style_of_unit;

	function orientation_of_unit (
	-- Returns the orientation of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name : in type_unit_name.bounded_string; -- the unit being inquired
		units : in et_schematic.type_units.map) -- the list of units
		return type_angle is
		unit_cursor : et_schematic.type_units.cursor;
	begin
		unit_cursor := et_schematic.type_units.find (container => units, key => name);
		return et_schematic.type_units.element (unit_cursor).orientation;
	end orientation_of_unit;
	

	
	procedure write_unit_properties (
	-- Writes the properties of the unit indicated by the given cursor.
		unit			: in type_units.cursor;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_coordinates;
	begin
		log_indentation_up;
		
		-- unit name
		log ("unit " 
			& to_string (type_units.key (unit)), log_threshold);

		log_indentation_up;
		
		-- alternative representation
		log ("alternative (deMorgan) representation " 
			 & to_lower (type_alternative_representation'image (type_units.element (unit).alt_repres)),
			 log_threshold + 1);

		-- timestamp
		log ("timestamp " 
			& string (type_units.element (unit).timestamp), log_threshold + 1);

		-- position
		log (to_string (position => type_units.element (unit).position), log_threshold + 1);

		-- orientation or angle
		log (to_string (type_units.element (unit).orientation), log_threshold + 1);

		-- mirror style
		log (to_string (type_units.element (unit).mirror), log_threshold + 1);

		
		-- placeholders
		log ("placeholders", log_threshold + 1);
		log_indentation_up;

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).reference,
				log_threshold => log_threshold + 1);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).value,
				log_threshold => log_threshold + 1);

			-- some placeholders exist depending on the component appearance
			case type_units.element (unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).packge,
						log_threshold => log_threshold + 1);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).datasheet,
						log_threshold => log_threshold + 1);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).purpose,
						log_threshold => log_threshold + 1);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).partcode,
						log_threshold => log_threshold + 1);

					-- bom
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element (unit).bom,
						log_threshold => log_threshold + 1);
					
				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).commissioned,
				log_threshold => log_threshold + 1);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).updated,
				log_threshold => log_threshold + 1);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element (unit).author,
				log_threshold => log_threshold + 1);

		log_indentation_down;
		log_indentation_down;
		log_indentation_down;		
	end write_unit_properties;

	function length (segment : in type_net_segment) return type_distance is
	-- Returns the length of the given net segment.
		len : type_distance;
		use et_string_processing;
	begin
		len := distance (segment.coordinates_start, segment.coordinates_end);
		log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
		return len;
	end length;
	
-- 	procedure write_coordinates_of_segment (segment : in type_net_segment) is
-- 	-- Writes the start and end coordinates of a net segment.
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 	
-- 		log_threshold : type_log_level := 1;
-- 	begin
-- 		log_indentation_up;
-- 		
-- 		log ("start "
-- 			& to_string (position => segment.coordinates_start)
-- 			& " end " 
-- 			& to_string (position => segment.coordinates_end),
-- 			level => log_threshold
-- 			);
-- 		
-- 		log_indentation_down;
-- 	end write_coordinates_of_segment;

	function to_string (segment : in type_net_segment; scope : in type_scope := sheet) return string is
	-- Returns the start and end coordinates of the given net segment.
	begin
		-- CS: it is sufficient to output
		return ("start "
			& to_string (position => segment.coordinates_start, scope => scope)
			& " end " 
			& to_string (position => segment.coordinates_end, scope => xy));
	end to_string;

	function to_string (scope : in type_strand_scope) return string is
	-- Retruns the given scope as string.
	begin
		--return to_lower (type_scope_of_net'image (scope));
		return type_strand_scope'image (scope);
	end to_string;

	function lowest_xy (strand : in type_strand) return type_2d_point is
	-- Returns the lowest x/y position of the given strand.
		point_1, point_2 : type_2d_point;
		segment : type_net_segments.cursor;
	
		use type_net_segments;
		use et_string_processing;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance
	begin
		log_indentation_up;
		log ("calculating the point nearest to drawing origin ...", level => 3);

		-- init point_1 as the farest possible point from drawing origin
		set_x (point_1, type_distance_xy'last);
		set_y (point_1, type_distance_xy'last);
		
		-- loop through segments and keep the nearest point to origin
		segment := strand.segments.first;
		while segment /= type_net_segments.no_element loop

			-- check start point of segment
			-- if closer to orign than point_1 keep start point
			point_2	:= type_2d_point (element (segment).coordinates_start);
			if distance (point_2, zero) < distance (point_1, zero) then
				log (" start", level => 4);
				point_1 := point_2;
			end if;

			-- check start point of segment
			-- if closer to orign than point_1 keep end point
			point_2	:= type_2d_point (element (segment).coordinates_end);
			if distance (point_2, zero) < distance (point_1, zero) then
				log (" end", level => 4);
				point_1 := point_2;
			end if;
			
			next (segment);
		end loop;

		log_indentation_down;
		
		return point_1;
	end lowest_xy;
	
	procedure add_strand (
	-- Adds a strand into the the module (indicated by module_cursor).
		strand : in et_schematic.type_strand) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is
			use et_string_processing;
		begin
			log_indentation_up;
			log (text => "inserting strand " & to_string (strand.name) & " in database ...", level => 3);
			log_indentation_down;

			module.strands.append (strand);
		end add;
		
	begin -- add_strand
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_strand;

	
-- 	procedure write_coordinates_of_junction (junction : in type_net_junction) is
-- 	-- Writes the coordinates of a net junction.
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 	
-- 		log_threshold : type_log_level := 1;
-- 	begin
-- 		log_indentation_up;
-- 		
-- 		log (to_string (junction.coordinates),
-- 			 level => log_threshold
-- 			); 
-- 		
-- 		log_indentation_down;
-- 	end write_coordinates_of_junction;			


	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment'class) 
		return boolean is

		-- CS: clean up as in port_sits_on_segment
		
		zero : constant et_coordinates.type_distance := et_coordinates.zero_distance;
		sits_on_segment : boolean := false;
		d : et_geometry.type_distance_point_from_line;

		use et_geometry;
		use et_coordinates;

	begin
		-- calculate the shortes distance of point from line.
		d := distance_of_point_from_line (
			point 		=> type_2d_point (junction.coordinates),
			line_start	=> type_2d_point (segment.coordinates_start),
			line_end	=> type_2d_point (segment.coordinates_end),
			line_range	=> inside_end_points);

		if (not d.out_of_range) and d.distance = zero then
			sits_on_segment := true;
		end if;

		return sits_on_segment;
	end junction_sits_on_segment;
	
	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character.
	-- NOTE: Leading zeroes in the id are removed.
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false		
		) return type_component_reference is

		r : type_component_reference := (
				prefix => type_component_prefix.to_bounded_string(""),
				id => 0,
				id_width => 1);
	
		c : character;
		p : type_component_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (text => latin_1.lf & message_error & "invalid component reference '" & text_in & "'",
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_component_prefix;
	begin
		-- assemble prefix
		for i in text_in'first .. text_in'last loop
			c := text_in(i);
			
			case i is 
				-- The first character MUST be an upper case letter.
				-- If allow_special_charater_in_prefix then the first letter is
				-- allowed to be a special character. (kicad uses '#' for power symbols)
				when 1 => 
					case allow_special_character_in_prefix is
						when false =>
							if is_upper(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
							if is_upper(c) or is_special(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;
					end case;
					
				-- Further characters are appended to prefix if they are upper case letters.
				-- If a upper-case-letter is found, the prefix is assumed as complete.
				-- A lower case letter will later be detetect when assembling the component id.
				when others =>
					if is_upper(c) then
						r.prefix := r.prefix & c;
					else
						--put("   prefix " & type_component_prefix.to_string(r.prefix));
						-- CS: check if allowed prefix
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in'last loop
			c := text_in(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
-- 		put_line(" id    " & natural'image(r.id));
-- 		put_line(" digits" & natural'image(r.id_width));
		
		return r;
	end to_component_reference;

	function to_string ( reference : in type_component_reference) return string is
	-- Returns the given component reference as string.
	-- Prepends leading zeros according to reference.id_width.
		id_width_wanted	: natural := reference.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim(natural'image(reference.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz				: natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return (type_component_prefix.to_string(reference.prefix) 
					& trim(natural'image(reference.id),left));
				
			when others => -- leading zeros required
				return (type_component_prefix.to_string(reference.prefix) 
					& lz * '0' & trim(natural'image(reference.id),left));
		end case;
	end to_string;


	function compare_reference ( left, right : in type_component_reference) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix.
		-- Example: If left is C201 and right is R4 then the result is true as C comes before R.

		if left.prefix < right.prefix then -- like C201 and R4
			result := true;
		elsif left.prefix > right.prefix then -- like R4 and C201
			result := false;
		elsif left.prefix = right.prefix then -- like IC33 and IC34

			-- If equal prefixes, we compare the id:
			if left.id < right.id then -- like 33 and 34
				result := true;
			else
				result := false; -- like 34 and 33
			end if;

		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_reference;

	function equal_reference (left, right : in type_component_reference) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix. If prefixes are equal, we compare the id.
		-- If either of them does not match, the result is set false.
		if left.prefix = right.prefix then -- like IC and IC

			if left.id = right.id then -- like 4 and 4
				result := true;
			else -- like 5 and 6
				result := false;
			end if;
			
		else -- like R and IC
			result := false; 
		end if;

		return result;
	end equal_reference;
	


	procedure first_module is
	-- Resets the module_cursor to the first module of the rig.
	begin
		module_cursor := rig.first;
		-- CS: exception handler in case given module does not exist
	end first_module;
	

	procedure set_module (
	-- Sets the active module. Leaves module_cursor pointing
	-- to the module.
		module_name : in et_coordinates.type_submodule_name.bounded_string) is
	begin
		module_cursor := rig.find (module_name);
		-- CS: exception handler in case given module does not exist
	end set_module;

	
	procedure add_module (
	-- Adds a module into the rig. Leaves module_cursor pointing
	-- to the module inserted last.
		module_name : in et_coordinates.type_submodule_name.bounded_string;
		module		: in type_module) is
		
		inserted : boolean := false;
	begin
		rig.insert (
			key			=> module_name,
			new_item	=> module,
			position	=> module_cursor,
			inserted	=> inserted
			);

		if not inserted then
			null; -- CS: error message
			raise constraint_error;
		end if;
	end add_module;


	procedure add_gui_submodule (
	-- Inserts a gui submodule in the module (indicated by module_cursor)
		name		: in et_coordinates.type_submodule_name.bounded_string;
		gui_sub_mod	: in et_schematic.type_gui_submodule) is

		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_gui_submodules.cursor;

			use et_string_processing;
		begin
			module.submodules.insert (
				key			=> name,
				new_item	=> gui_sub_mod,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then
				if log_level >= 1 then
                    null; -- CS: write this procedure:
                    log ("hierachic sheet", console => true);
					--et_schematic.write_gui_submodule_properties (gui_sub_mod => cursor);
				end if;
			else -- not inserted. net already in module -> abort
				null; -- CS: 
				raise constraint_error;
			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_gui_submodule;

	
	procedure add_sheet_header (
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string) is

		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.sheet_headers.insert (
				key			=> sheet,
				new_item	=> header);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_header
			end if;

		end add;

	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_sheet_header;

	
	procedure add_frame (
	-- Inserts a drawing frame in the the module (indicated by module_cursor).
	-- As drawing frames are collected in a simple list, the same frame
	-- can be added multiple times.
		frame	: in et_schematic.type_frame) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.frames.append (
				new_item	=> frame);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_frame_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_frame;

	
	
	procedure add_title_block (
	-- Inserts a title block in the the module (indicated by module_cursor).
	-- As title blocks are collected in a simple list, the same title block
	-- can be added multiple times.
		tblock	: in et_schematic.type_title_block) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.title_blocks.append (
				new_item	=> tblock);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_title_block_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_title_block;


	procedure add_note (
	-- Inserts a note in the the module (indicated by module_cursor).
	-- As notes are collected in a simple list, the same note
	-- can be added multiple times.
		note	: in et_schematic.type_note) is
		
		procedure add (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.notes.append (
				new_item	=> note);

			if log_level >= 1 then
				null; -- CS: 
				--et_schematic.write_note_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_note;

	function to_string (mirror : in type_mirror) return string is
	-- returns the given mirror style as string
	begin
		return "mirror style " & to_lower (type_mirror'image (mirror));
	end to_string;
	
	function first_strand return type_strands.cursor is
	-- Returns a cursor pointing to the first strand of the module (indicated by module_cursor).
		cursor : type_strands.cursor;	

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.strands.first;
		end set_cursor;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_strand;

	procedure rename_strands (
	-- Renames all strands with the name_before to the name_after.
	-- Changes the scope of the affected strands to "global".
		name_before		: in type_net_name.bounded_string;
		name_after		: in type_net_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;

		count : natural := 0;
	
		procedure rename (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_schematic.type_strands;
			
			cursor : type_strands.cursor := module.strands.first;
			-- Points to the strand being processed
			
			renamed : boolean := false; -- signals that a strand renaming took place
			-- Used to abort renaming after an anonymous strands has been renamed.
			-- The names of anonymous strands like (N$6) are unique. So after the first
			-- renaming the procedure comes to an early end.

			procedure do_it (strand : in out type_strand) is
			begin
				-- search for the strand to be renamed
				if strand.name = name_before then

					count := count + 1;
					log_indentation_up;
-- 					log (
-- 						text => to_string (strand.name) 
-- 							& " to "
-- 							& to_string (name_after) & " ...",
-- 							level => 2
-- 						);
					log (to_string (position => strand.coordinates, scope => et_coordinates.module),
						log_threshold + 1);

					log_indentation_down;

					strand.name := name_after; -- assign new name to strand
					strand.scope := global;

					renamed := true; -- signal that renaming took place
				end if;
			end do_it;
			
		begin -- rename
			while cursor /= type_strands.no_element loop
				module.strands.update_element (
					position => cursor,
					process => do_it'access);

				-- Exit prematurely if name_before was anonymous. anonymous strand names are unique.
				-- So it is ok to exit prematurely.
				if renamed and anonymous (name_before) then
					exit;
				end if;
				
				next (cursor);
			end loop;
		end rename;
		
	begin -- rename_strands
		log ("renaming strands from " & to_string (name_before)
			 & " to " & to_string (name_after) & " ...", log_threshold);
		
		rig.update_element (
			position	=> module_cursor,
			process		=> rename'access
			);

		if count > 0 then
			log ("renamed" & natural'image (count) & " strands", log_threshold);
		else
			-- CS: This should never happen
			log_indentation_reset;
			log (message_error & "strand " 
				& to_string (name_before) & " not found !");
			raise constraint_error;
		end if;
	end rename_strands;

	procedure write_strands is
	-- Writes a nice overview of strands, net segments and labels
		use et_string_processing;

		procedure query_label (
			segment		: in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			log_indentation_up;
			while label_simple /= type_simple_labels.no_element loop
				log ("simple label " & to_string (position => element (label_simple).coordinates));
				next (label_simple);
			end loop;

			while label_tag /= type_tag_labels.no_element loop
				log ("tag label " & to_string (position => element (label_tag).coordinates));
				next (label_tag);
			end loop;

			log_indentation_down;
		end query_label;
	
		procedure query_segment (
			strand	: in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;
		begin
			while segment /= type_net_segments.no_element loop
				log_indentation_up;
				log ("segment " & to_string (element (segment)));

				type_net_segments.query_element (
					position	=> segment,
					process		=> query_label'access);

				log_indentation_down;				
				next (segment);
			end loop;
		end query_segment;
	
		procedure query_strands (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
			strand : type_strands.cursor := module.strands.first;
			use type_strands;
		begin
			while strand /= type_strands.no_element loop
				log_indentation_up;

				log (to_string (element (strand).name) & " scope " & to_string (element (strand).scope)
					& " in " 
					& et_coordinates.to_string (et_coordinates.path (element (strand).coordinates), top_module => false)
					& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates)));

-- 				log (et_coordinates.to_string (et_coordinates.path (element (strand).coordinates), top_module => false)
-- 					& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
-- 					& hierarchy_separator 
-- 					& to_string (element (strand).name) & " scope " & to_string (element (strand).scope)
-- 					);

				type_strands.query_element (
					position	=> strand,
					process		=> query_segment'access);
				
				log_indentation_down;
				next (strand);
			end loop;
		end query_strands;
		
	begin -- write_strands
		log ("creating net strands report ...");
		
		type_rig.query_element (
			position	=> module_cursor,
			process		=> query_strands'access);
	end write_strands;

	procedure check_strands is -- CS: currently not used
	-- Checks scope of strands across the current module (indicated by module_cursor)
	-- CS: describe the purpose of this procdure more detailled.
		use et_string_processing;

		procedure query_strands (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
			
			strand_primary		: type_strands.cursor := module.strands.first;
			strand_secondary	: type_strands.cursor;
			use type_strands;
		begin
			log ("checking local strands ...");
			
			while strand_primary /= type_strands.no_element loop
				log_indentation_up;
				
				if element (strand_primary).scope = local then

					log (
						text => to_string (element (strand_primary).name) 
							& " at " & to_string (
								position => element (strand_primary).coordinates,
								scope => et_coordinates.module),
						 level => 2);
						
					if strand_primary /= module.strands.last then
						strand_secondary := next (strand_primary);

						while strand_secondary /= type_strands.no_element loop
							if element (strand_secondary).scope = local then
								if sheet (element (strand_secondary).coordinates) /= sheet (element (strand_primary).coordinates) then
									
									if element (strand_secondary).name = element (strand_primary).name then

										put_line (standard_output, message_error & "net name conflict with net "
											& to_string (element (strand_secondary).name));

										log_indentation_reset;
										log (message_error & "net name conflict with net " 
											& to_string (element (strand_secondary).name) 
											& " at " & to_string (
												position => element (strand_secondary).coordinates,
												scope => et_coordinates.module));

										-- Explain the reason for this rule:
										log ("A local net is restricted to a single sheet ! Use global nets instead.");
								
										raise constraint_error;

									end if;
								end if;
							end if;
							
							next (strand_secondary);
						end loop;
					end if;
				end if;
					
				
				log_indentation_down;
				next (strand_primary);
			end loop;
		end query_strands;

	begin
		log ("checking strands ...");

		type_rig.query_element (
			position	=> module_cursor,
			process		=> query_strands'access);

	end check_strands;
	
	
	function first_segment (cursor : in type_strands.cursor) return type_net_segments.cursor is
	-- Returns a cursor pointing to the first net segment of the given strand.
		segment_cursor : type_net_segments.cursor;

		procedure set_cursor (
			strand : in type_strand) is
		begin
			segment_cursor := strand.segments.first;
		end set_cursor;

	begin
		type_strands.query_element (
			position	=> cursor,
			process		=> set_cursor'access
			);
		return segment_cursor;
	end first_segment;

	
	procedure link_strands (log_threshold : in et_string_processing.type_log_level) is
	-- Links strands to nets (see type_module.nets).

	-- Builds the nets (see type_module.nets) of the current module from its strands (see type_module.strands).
	-- NOTE: This is NOT about generating or exporting a netlist. See package et_netlist instead.
	-- This procdure should be called AFTER netlist generation because some strands may have changed their name.
	-- (Names of strands have changee due to power-out ports connected with them.)

	-- Build the module nets. Build_nets merges the strands which are still independed of
	-- each other. For example a strand named "VCC3V3" exists on submodule A on sheet 2. 
	-- Another strand "VCC3V3" exists on submodule C on sheet 1. They do not "know" each other
	-- and must be merged into a single net.
		use et_string_processing;
		use et_schematic.type_strands;
		use et_schematic.type_rig;

		strand : type_strands.cursor;

        type type_hierachic_net (available : boolean) is record
            case available is
                when true =>
                    submodule   : type_submodule_name.bounded_string;
                    net         : type_net_name.bounded_string;
                when false => null;
            end case;
        end record;
    
        function hierachic_net (strand : in type_strands.cursor) return type_hierachic_net is
        -- Returns the name of a subordinated hierarchic net (if available).
        -- If no hierarchic net available, returns a single "false". See type_hierachic_net specification.    
            segment : type_net_segments.cursor;
            segments :type_net_segments.list; -- workaround for possible gnat bug. see comments below
            use type_net_segments;
        
            gui_submodule : type_gui_submodules.cursor;
            gui_submodules : type_gui_submodules.map;  -- workaround for possible gnat bug. see comments below
            use type_gui_submodules;
        
            use et_schematic.type_rig;
            use et_coordinates.type_path_to_submodule;

            port : type_gui_submodule_ports.cursor;
            ports : type_gui_submodule_ports.map;  -- workaround for possible gnat bug. see comments below
            use type_gui_submodule_ports;

            function on_segment (port : in type_gui_submodule_port; segment : in type_net_segment) return boolean is
            -- Returns true if given port sits on given segment.
                use et_geometry;
                distance : type_distance_point_from_line;
            begin
                distance := distance_of_point_from_line (
                    point 		=> port.coordinates,
                    line_start	=> type_2d_point (segment.coordinates_start),
                    line_end	=> type_2d_point (segment.coordinates_end),
                    line_range	=> with_end_points);

                if not distance.out_of_range and distance.distance = zero_distance then
                    return true;
                else
                    return false;
                end if;
            end on_segment;

        begin -- hierachic_net
            -- If the design is flat -> nothing to do. If gui submodules available fetch them one after another.
            if not is_empty (element (module_cursor).submodules) then

                -- Loop in gui submodules:
                
                -- gui_submodule := element (module_cursor).submodules.first; -- THIS CURSOR DOES NOT WORK !
                -- NOTE: advancing cursor gui_submodule via statement "next (gui_submodule)" does not work with GNATMAKE 7.2.1 20171020 [gcc-7-branch revision 253932]
                -- see comment below
                -- As a workaround we work with a local copy of all submodules in variable gui_submodules and 
                -- advance the cursor "gui_submodule" there.
                gui_submodules := element (module_cursor).submodules;
                gui_submodule := gui_submodules.first;

                while gui_submodule /= type_gui_submodules.no_element loop

                    -- If gui_submodule and given strand are in the same submodule
                    if path (element (gui_submodule).coordinates) = path (element (strand).coordinates) then
                        -- CS: compare sheet ? If a submodule schematic may have lots of sheets, no need to do so ?

                        -- loop in segments of given strand:
                        
                        -- segment := element (strand).segments.first; -- THIS CURSOR DOES NOT WORK !
                        -- NOTE: advancing cursor segment via statement "next (segment)" does not work with GNATMAKE 7.2.1 20171020 [gcc-7-branch revision 253932]
                        -- As a workaround we work with a local copy of all segments in variable segments and advance a 
                        -- cursor "segment" there.
                        segments := element (strand).segments;
                        segment := segments.first;
                        while segment /= type_net_segments.no_element loop

                            -- loop in ports of gui_submodule
                            
                            -- port := element (gui_submodule).ports.first; -- THIS CURSOR DOES NOT WORK !
                            ports := element (gui_submodule).ports;
                            port := ports.first;
                            while port /= type_gui_submodule_ports.no_element loop

                                -- test if port sits on segment
                                if on_segment (element (port), element (segment)) then

                                    -- return submodule and net name
                                    return (
                                        available   => true,
                                        submodule   => key (gui_submodule),
                                        net         => type_net_name.to_bounded_string (to_string (key (port))));
                                end if;
                                
                                next (port);
                            end loop;

                            next (segment);
                        end loop;

                    end if;

                    next (gui_submodule);

                end loop;
            end if;
            
            return (available => false);
        end hierachic_net;


    
		procedure add_net (
		-- Creates a net with the name and the scope (local, hierarchic, global) of the current named_strand. 
		-- If named_strand is local, the net name is rendered to a full hierarchic name.
		-- If the net existed already, then named_strand is appended to the strands of the net.
		-- The scope of the named_strand is checked aginst the scope of the latest strand
		-- of the net.
			mod_name : in type_submodule_name.bounded_string;
			module   : in out type_module) is

			use et_schematic.type_nets;
			
			net_created : boolean;
			net_cursor : type_nets.cursor;
			net_name : type_net_name.bounded_string;

			procedure add_strand (
				net_name : in type_net_name.bounded_string;
				net		 : in out type_net) is

-- 				sub_strand : type_strands.cursor;
			begin
				log ("strand of net " & to_string (net_name), level => log_threshold + 2);
				
				if net_created then -- net has just been created
					net.scope := element (strand).scope; -- set scope of net
				end if;

				if log_level >= log_threshold + 2 then
					log_indentation_up;
					log ("strand at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module));
					log_indentation_down;
				end if;
				
				-- add named_strand to the net
				net.strands.append (new_item => element (strand));


				-- CS: search for hierarchical sheets connected with this strand
-- 				if hierachic_net (strand).available then
-- 					log_indentation_up;
--                     --log ("-> " & to_string (key (port)));
--                     log ("has hierarchic ports", log_threshold + 2);
-- 					log_indentation_down;
-- 				end if;
					
			end add_strand;

			procedure create_net is
			begin
				-- create net
				module.nets.insert (
					--key 		=> element (named_strand).name,
					key 		=> net_name,
					position	=> net_cursor,
					inserted	=> net_created);

				-- If net created or already there, net_cursor points to the net where the strand is to be added.
				module.nets.update_element (
					position	=> net_cursor,
					process		=> add_strand'access);
			end create_net;
			
		begin -- add_net

			-- form the net name depending on scope
			case element (strand).scope is

				when local =>
					
					-- Output a warning if strand has no name.
					if anonymous (element (strand).name) then
						log (message_warning & "net " & to_string (element (strand).name) & " has no dedicated name !");
						-- CS: Show the lowest xy position
						log ("at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module));
					end if;

					-- For local strands the full hierarchic name of the net must be formed
					-- in order to get something like "driver.GND" :
					net_name := type_net_name.to_bounded_string (
						et_coordinates.to_string (et_coordinates.path (element (strand).coordinates), top_module => false)
							& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
							& et_coordinates.hierarchy_separator & et_schematic.to_string (element (strand).name));

					create_net;
					
				when global =>
					net_name := element (strand).name;
					create_net;
					
				-- CS: special threatment for hierarchic strands ?
				when hierarchic =>
					log (message_error & "hierarchical nets not supported !");
					raise constraint_error;

				when unknown =>
					log (message_error & "unknown scope of net !");
					raise constraint_error; -- CS: should never happen as all strands should have a scope by now

			end case;

		end add_net;

	begin -- link_strands
		log (text => "linking strands to nets ...", level => log_threshold);

		log_indentation_up;

		-- loop in strands of the current module
		strand := first_strand;
		log_indentation_up;
		while strand /= type_strands.no_element loop

			-- Create net and append strand to module.nets
			rig.update_element (
				position => module_cursor,
				process => add_net'access);
				
			next (strand);
		end loop;
		log_indentation_down;
			
		-- show a net report -- CS: separate procedure
		if log_level >= log_threshold + 1 then
			write_nets;
		end if;
		
		log_indentation_down;
	end link_strands;

	procedure write_nets is
	-- Writes a nice overview of all nets, strands, segments and labels.
	-- Bases on the element "nets" of the modules. See specification of type_module.
		use et_string_processing;
	
		procedure query_label (
			segment : in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			log_indentation_up;
			while label_simple /= type_simple_labels.no_element loop
				log ("simple label at " & to_string (position => element (label_simple).coordinates, scope => xy));
				next (label_simple);
			end loop;

			while label_tag /= type_tag_labels.no_element loop
				if element (label_tag).hierarchic then
					log ("hierarchic label at " 
						& to_string (position => element (label_tag).coordinates, scope => xy));
				end if;

				if element (label_tag).global then
					log ("global label at " 
						& to_string (position => element (label_tag).coordinates, scope => xy));
				end if;
				
				next (label_tag);
			end loop;
			
			log_indentation_down;
		end query_label;
		
		procedure query_segment (
			strand : in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;

			-- for the segment we provide a consequtive number which has no further meaning
			segment_number : count_type := 1;			
		begin
			log_indentation_up;
			while segment /= type_net_segments.no_element loop
				log ("segment" 
					 & count_type'image (segment_number) 
					 & latin_1.space
					 & to_string (segment => element (segment), scope => xy));

				query_element (
					position => segment,
					process => query_label'access);
				
				segment_number := segment_number + 1;
				next (segment);
			end loop;
			log_indentation_down;
		end query_segment;
		
		procedure query_strand (
			net_name : in type_net_name.bounded_string;
			net : in type_net) is
			
			strand : type_strands.cursor := net.strands.first;
			use type_strands;

			-- for the strand we provide a consequtive number which has no further meaning
			strand_number : count_type := 1;			
		begin -- query_strand
			log_indentation_up;
			while strand /= type_strands.no_element loop
				log ("strand #" & trim (count_type'image (strand_number), left) &
					" at " & to_string (position => element (strand).coordinates, scope => et_coordinates.module)
					);

				query_element (
					position => strand,
					process => query_segment'access);
				
				strand_number := strand_number + 1;
				next (strand);
			end loop;

			log_indentation_down;
		end query_strand;
		
		procedure query_net (
			mod_name : in type_submodule_name.bounded_string;
			module : in type_module) is
			net : type_nets.cursor := module.nets.first;

			use type_nets;
		begin
			log_indentation_up;
			while net /= type_nets.no_element loop
				log ("net " & to_string (key (net)));

				query_element (
					position => net,
					process => query_strand'access);
				
				next (net);
			end loop;

			log_indentation_down;
		end query_net;

		use type_rig;
		
	begin -- write_nets
		log ("creating net report ...");
		log_indentation_up;
			
		first_module;
		while module_cursor /= type_rig.no_element loop
				
			log ("module " & to_string (key (module_cursor)));

			query_element (
				position => module_cursor,
				process => query_net'access);
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end write_nets;

		
	function first_component return type_components.cursor is
	-- Returns a cursor pointing to the first component of the module (indicated by module_cursor).
		cursor : type_components.cursor;

		procedure set_cursor (
			mod_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.components.first;
		end set_cursor;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_component;

	
	
	
	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
	-- If a component is already in the list, nothing happens.
	-- Components may occur multiple times in the schematic if they 
	-- consist of more than one unit.
	-- CS: This assumption may not apply for all CAE systems. Currently we
	-- consider only kicad. In other cases the "inserted" check (see below) 
	-- must be enabled via an argument.
		reference		: in et_libraries.type_component_reference;
		component		: in type_component;
		log_threshold 	: in et_string_processing.type_log_level) is
		
		procedure add (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_components.cursor;

			use et_string_processing;
		begin
			module.components.insert (
				key			=> reference,
				new_item	=> component,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

-- 			if inserted then -- first occurence of component
				write_component_properties (component => cursor, log_threshold => log_threshold + 1);
-- 			else -- not inserted
-- 				null; -- CS: see comment above
				--raise constraint_error;
-- 			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_component;
	
	procedure add_unit (
	-- Adds a unit into the given commponent.
		reference		: in et_libraries.type_component_reference;
		unit_name		: in et_libraries.type_unit_name.bounded_string;
		unit 			: in type_unit;
		log_threshold	: in et_string_processing.type_log_level) is

		procedure add (
			reference	: in et_libraries.type_component_reference;
			component	: in out type_component) is

			inserted	: boolean := false;
			cursor		: type_units.cursor;

			use et_string_processing;
		begin
			component.units.insert (
				key			=> unit_name,
				new_item	=> unit,
				position	=> cursor, -- updates unit_cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then -- fine. unit was inserted successfully
-- 				if log_level >= 2 then				
					write_unit_properties (unit => cursor, log_threshold => log_threshold);
-- 				end if;
			else -- not inserted, unit already in component -> failure
				log_indentation_reset;
				log (
					text => message_error & "multiple occurence of the same unit !",
					console => true);
				raise constraint_error;
			end if;
		end add;
		
		procedure locate_component (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			cursor : type_components.cursor;
		begin
			cursor := module.components.find (reference);
			-- CS: do something if reference not found
			
			module.components.update_element (
				position	=> cursor,
				process		=> add'access
				);
		end locate_component;
		
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> locate_component'access
			);
	end add_unit;

	procedure reset_component_cursor (cursor : in out type_components.cursor) is
	-- Resets the given component cursor to the begin of the component list.
		procedure reset (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components;
		begin
			cursor := first (module.components);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_component_cursor;

	procedure reset_library_cursor (cursor : in out type_full_library_names.cursor) is
	-- Resets the given library cursor to the begin of the library list list.
		procedure reset (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			cursor := first (module.libraries);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_library_cursor;

	function number_of_libraries return count_type is
	-- Returns the number of project libraries.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			n := length (module.libraries);
		end get;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end number_of_libraries;

	function units_of_component (component_cursor : in type_components.cursor) return type_units.map is
	-- Returns the units of the given component.
		u : type_units.map;

		procedure locate (
			name : in type_component_reference;
			component : in type_component) is
		begin
			-- copy the units of the component to the return value
			u := component.units;
		end locate;
		
	begin
		-- locate the given component by component_cursor
		type_components.query_element (component_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return u;
	end units_of_component;


-- 	procedure warning_on_name_less_net (
-- 	-- Writes a warning about a name-less net.
-- 		name 	: in et_schematic.type_net_name.bounded_string;
-- 		net		: in et_schematic.type_strand
-- 		) is
-- 
-- 		use et_string_processing;
-- 	begin
-- 		log (
-- 			text => message_warning & "name-less net " & to_string (name) & " found !"
-- 			);
-- 		-- CS: output coordinates of net (lowest x/Y)
-- 	end warning_on_name_less_net;
	

	
-- BOM
	
	procedure make_bom is
	-- Generates a bom file. This file is csv formatted and is to be processed by
	-- other ERP tools (like stock_manager, see <https://github.com/Blunk-electronic/stock_manager>)
		use ada.directories;
		use et_general;
		use et_string_processing;
		use et_export;
		use et_csv;
		use type_rig;
		use type_components;
		
		bom_file_name : type_bom_file_name.bounded_string;
		bom_handle : ada.text_io.file_type;

		column_component	: constant string (1 .. 9) := "COMPONENT";
		column_value		: constant string (1 .. 5) := "VALUE";
		column_generic_name	: constant string (1 ..12) := "GENERIC_NAME";
		column_package		: constant string (1 .. 7) := "PACKAGE";
		column_author		: constant string (1 .. 6) := "AUTHOR";
		column_bom			: constant string (1 .. 3) := "BOM";
		column_commissioned	: constant string (1 ..12) := "COMMISSIONED";
		column_purpose		: constant string (1 .. 7) := "PURPOSE";
		column_part_code	: constant string (1 .. 9) := "PART_CODE"; -- CS: make sure stock_manager can handle it. former PART_CODE_BEL
		column_part_code_ext: constant string (1 ..13) := "PART_CODE_EXT"; -- not used
		column_updated		: constant string (1 .. 7) := "UPDATED";

		component_cursor : type_components.cursor;
		
	begin
		first_module;
		
		log (text => "writing BOM ...", level => 1);

		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);

			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
			bom_file_name := type_bom_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_bom)
				);

			-- create the netlist (which inevitably and intentionally overwrites the previous file)
			log_indentation_up;
			log (text => "creating BOM file " & type_bom_file_name.to_string (bom_file_name), level => 1);
			create (
				file => bom_handle,
				mode => out_file, 
				name => type_bom_file_name.to_string (bom_file_name));

			-- CS: A nice header should be placed. First make sure stock_manager can handle it.

			-- write the BOM table header
			put_field (file => bom_handle, text => column_component);
			put_field (file => bom_handle, text => column_value);
			put_field (file => bom_handle, text => column_generic_name);
			put_field (file => bom_handle, text => column_package);
			put_field (file => bom_handle, text => column_author);
			put_field (file => bom_handle, text => column_bom);
			put_field (file => bom_handle, text => column_commissioned);
			put_field (file => bom_handle, text => column_purpose);
			put_field (file => bom_handle, text => column_part_code);
			put_field (file => bom_handle, text => column_part_code_ext);
			put_field (file => bom_handle, text => column_updated);
			put_lf    (file => bom_handle);

			log_indentation_up;
			component_cursor := first_component;
			while component_cursor /= type_components.no_element loop

				-- We ignore all virtual components like power flags, power symbols, ...
				if component_appearance (component_cursor) = sch_pcb then

					if bom (component_cursor) = YES then
						log (text => to_string (key (component_cursor)), level => 1);

						put_field (file => bom_handle, text => to_string (key (component_cursor)));
						put_field (file => bom_handle, text => to_string (element (component_cursor).value));
						put_field (file => bom_handle, text => to_string (element (component_cursor).name_in_library));
						put_field (file => bom_handle, text => to_string (element (component_cursor).variant.variant.packge));
						put_field (file => bom_handle, text => to_string (element (component_cursor).author));
						put_field (file => bom_handle, text => to_string (element (component_cursor).bom));
						put_field (file => bom_handle, text => to_string (element (component_cursor).commissioned));
						put_field (file => bom_handle, text => to_string (element (component_cursor).purpose));
						put_field (file => bom_handle, text => to_string (element (component_cursor).partcode));

						-- CS: This is an empty field. it is reserved for the attribute "PART_CODE_EXT" 
						-- which is currently not supported:
						put_field (file => bom_handle, text => "");

						put_field (file => bom_handle, text => to_string (element (component_cursor).updated));
						put_lf    (file => bom_handle);

					end if;
				end if;

				next (component_cursor);
			end loop;
			log_indentation_down;
			
			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
			-- put_line (bom_handle, comment_mark & " end of list");
			
			close (bom_handle);
			log_indentation_down;
			next (module_cursor);
		end loop;
		
	end make_bom;

	

-- STATISTICS

	function components_total return count_type is
	-- Returns the total number of components (inc. virtual components) in the module indicated by module_cursor.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components;
		begin
			n := length (module.components);
		end get;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_total;

	function components_real (mounted_only : in boolean := false) return count_type is
	-- Returns the number of real components in the module indicated by module_cursor.
	-- If mounted_only is true, only components with the flag "bom" set are adressed.
	-- If mounted_only is false, all real components are adressed.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is

			component : type_components.cursor;
			use type_components;
			use et_string_processing;
		begin -- get
			if mounted_only then
				log (text => "mounted components", level => 1);
			else
				log (text => "real components", level => 1);
			end if;

			component := module.components.first;			
			while component /= type_components.no_element loop

				-- filter real components
				if component_appearance (component) = sch_pcb then
					log_indentation_up;
					
					if mounted_only then
						if bom (component) = YES then
							log (text => to_string (key (component)), level => 1);
							n := n + 1;
						end if;
					else
						log (text => to_string (key (component)), level => 1);
						n := n + 1;
					end if;

					log_indentation_down;
				end if;
				next (component);
			end loop;

			if n = 0 then
				log_indentation_up;
				log (text => "none", level => 1);
				log_indentation_down;
			end if;
		end get;
	
	begin -- components_real
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_real;

	function components_virtual return count_type is
	-- Returns the number of virtual components in the module indicated by module_cursor.
		n : count_type := 0;

		procedure get (
			name	: in et_coordinates.type_submodule_name.bounded_string;
			module	: in type_module) is

			component : type_components.cursor;
			use type_components;
			use et_string_processing;
		begin
			log (text => "virtual components", level => 1);
			
			component := module.components.first;

			while component /= type_components.no_element loop
				
				-- filter real components
				if component_appearance (component) = sch then
					log_indentation_up;
					
					log (text => to_string (key (component)), level => 1);
					n := n + 1;
					
					log_indentation_down;
				end if;
				next (component);
			end loop;

			if n = 0 then
				log_indentation_up;
				log (text => "none", level => 1);
				log_indentation_down;
			end if;
			
		end get;
	
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end components_virtual;

	
	procedure make_statistics is
	-- Generates the statistics on components and nets of the rig.
	-- Breaks statistics up into submodules, general statistics (CAD) and CAM related things.
		statistics_file_name_cad: type_statistic_file_name.bounded_string;
		statistics_file_name_cam: type_statistic_file_name.bounded_string;
		statistics_handle_cad	: ada.text_io.file_type;
		statistics_handle_cam	: ada.text_io.file_type;

		component : type_components.cursor;
		-- CS net
		
		use ada.directories;
		use et_general;
		use type_components;
		use type_rig;
		use et_string_processing;
		use et_netlist;
	begin
		first_module;
		
		log (text => "writing statistics ...", level => 1);

		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);
			log_indentation_up;

			-- CAD
			-- compose the CAD statistics file name and its path like "../ET/motor_driver/motor_driver.stat"
			statistics_file_name_cad := type_statistic_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose (work_directory, to_string (key (module_cursor))),
					name => to_string (key (module_cursor)),
					extension => extension_statistics)
				);

			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
			log (text => "CAD statistics file " & type_statistic_file_name.to_string (statistics_file_name_cad), level => 1);
			create (
				file => statistics_handle_cad,
				mode => out_file, 
				name => type_statistic_file_name.to_string (statistics_file_name_cad));

			log_indentation_up;
			put_line (statistics_handle_cad, comment_mark & " " & system_name & " CAD statistics");
			put_line (statistics_handle_cad, comment_mark & " date " & string (date_now));
			put_line (statistics_handle_cad, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (statistics_handle_cad, comment_mark & " " & row_separator_double);

			-- components
			put_line (statistics_handle_cad, "components");
			put_line (statistics_handle_cad, " total  " & count_type'image (components_total) & " (incl. virtual components)");
			put_line (statistics_handle_cad, " real   " & count_type'image (components_real)); -- all real components ! Regardless of bom status !
			put_line (statistics_handle_cad, " virtual" & count_type'image (components_virtual) & " (power symbols, power flags, ...)");
			new_line (statistics_handle_cad);
			-- CS: resitors, leds, transitors, ...

			-- nets
			put_line (statistics_handle_cad, "nets");
			-- CS: currently we get the net and pin numbers from et_netlist.rig.
			-- In the future this data should be taken from et_schematic.rig.
			et_netlist.set_module (key (module_cursor));
			put_line (statistics_handle_cad, " total  " & count_type'image (et_netlist.net_count));
			-- As for the total number of ports, we take all ports into account as they are listed in et_netlist.rig.
			-- This includes ports of virtual components except so called "power_flags".
			put_line (statistics_handle_cad, "  ports " & count_type'image (et_netlist.component_ports_total) & " (excl. power flags)");
			
			-- finish statistics			
			put_line (statistics_handle_cad, comment_mark & " " & row_separator_single);
			put_line (statistics_handle_cad, comment_mark & " end of list");
			log_indentation_down;
			close (statistics_handle_cad);



			-- CAM
			-- compose the CAM statistics file name and its path like "../ET/motor_driver/CAM/motor_driver.stat"
			statistics_file_name_cam := type_statistic_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_statistics)
				);

			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
			log (text => "CAM statistics file " & type_statistic_file_name.to_string (statistics_file_name_cam), level => 1);
			create (
				file => statistics_handle_cam,
				mode => out_file, 
				name => type_statistic_file_name.to_string (statistics_file_name_cam));

			log_indentation_up;
			put_line (statistics_handle_cam, comment_mark & " " & system_name & " CAM statistics");
			put_line (statistics_handle_cam, comment_mark & " date " & string (date_now));
			put_line (statistics_handle_cam, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (statistics_handle_cam, comment_mark & " " & row_separator_double);

			-- components
			put_line (statistics_handle_cam, "components");
			put_line (statistics_handle_cam, " total" & count_type'image (components_real (mounted_only => true)));
			-- As for the total number of real component ports, we take all ports into account for which a physical
			-- pad must be manufactured. Here it does not matter if a component is to be mounted or not, if a pin is connected or not.
			-- CS: THT/SMD
			-- CS: THT/SMD/pins/pads
			-- CS: resitors, leds, transitors, ...
			new_line (statistics_handle_cam);

			-- nets
			put_line (statistics_handle_cam, "nets");
			-- CS: currently we get the net and pin numbers from et_netlist.rig.
			-- In the future this data should be taken from et_schematic.rig.
			et_netlist.set_module (key (module_cursor));
			put_line (statistics_handle_cam, " total" & count_type'image (et_netlist.net_count));

			-- finish statistics
			put_line (statistics_handle_cam, comment_mark & " " & row_separator_single);
			put_line (statistics_handle_cam, comment_mark & " end of list");
			log_indentation_down;
			close (statistics_handle_cam);



			
			log_indentation_down;
			next (module_cursor);
		end loop;
			
		
	end make_statistics;

	
end et_schematic;
-- Soli Deo Gloria
