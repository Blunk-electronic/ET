------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SCHEMATIC                             --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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
--		1. Objects like net segments, net labels, junctions, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_coordinates;			use et_coordinates;
with et_libraries;				use et_libraries;
with et_string_processing;


package et_schematic is

-- NAMES GENERAL

	-- The name of a module may have 100 characters which seems sufficient for now.
 	module_name_length : constant natural := 100;
	package type_module_name is new generic_bounded_length(module_name_length); use type_module_name;


    -- A sheet title may have 100 characters which seems sufficient for now.
 	sheet_title_length : constant natural := 100;    
	package type_sheet_title is new generic_bounded_length(sheet_title_length); use type_sheet_title;
    
    sheet_comment_length : constant natural := 100;
    package type_sheet_comment is new generic_bounded_length(sheet_comment_length); use type_sheet_comment;

	-- The name of a project may have 100 characters which seems sufficient for now.
 	project_name_length : constant natural := 100;
	package type_project_name is new generic_bounded_length (project_name_length); use type_project_name;
	schematic_file_name_length : constant positive := project_name_length + 4; -- includes extension
	package type_schematic_file_name is new generic_bounded_length (schematic_file_name_length); use type_schematic_file_name;

	schematic_handle	: ada.text_io.file_type;
	
	function to_string (schematic : in type_schematic_file_name.bounded_string) return string;
	-- Returns the given schematic file name as string.

	-- The project file name may have the same length as the project name itself plus extension (*.pro, *.prj, ...):
	package type_project_file_name is new generic_bounded_length (project_name_length + 4); use type_project_file_name;
	project_file_name	: type_project_file_name.bounded_string;
	project_file_handle	: ada.text_io.file_type;
	
	-- Sheet names may have the same length as schematic files.
	package type_sheet_name is new generic_bounded_length (schematic_file_name_length); use type_sheet_name;
   
-- PAPER SIZES
    type type_paper_size is ( A0, A1, A2, A4 ); -- CS: others
    paper_size_default : type_paper_size := A4;

	
-- COORDINATES
   
	-- Within a schematic every object can be located by the name of the:
    -- - path to the submodule (first item in path is the top level module)
	-- - submodule name
	-- - sheet number (NOTE: The sheet numbering restarts in a submodule)
	-- - basic coordinates x/y


	-- While reading submodules (in kicad sheets) the path_to_submodule keeps record of current point in the design 
	-- hierarchy. Each time a submodule ABC has been found with nested submodules, the name of ABC is appended here.
	-- Once the parent module is entered again, the name ABC is removed from the list. When assigning coordinates
	-- to an object, the path_to_submodule is read. 
	-- So this list (from first to last) provides a full path that tells us
	-- the exact location of the submodule within the design hierarchy.
	path_to_submodule : et_coordinates.type_path_to_submodule.list;
	
	-- Sometimes we need to output the location of a submodule:
	procedure write_path_to_submodule;

	-- Here we append a submodule name the the path_to_submodule.
	procedure append_name_of_parent_module_to_path (submodule : in et_coordinates.type_submodule_name.bounded_string);

	-- Here we remove the last submodule name form the path_to_submodule.
	procedure delete_last_module_name_from_path;



	-- CS: negative schematic coordinates should be forbidden	
-- 	type type_coordinates is new et_coordinates.type_2d_point with private;	
-- 	type type_coordinates is new et_libraries.type_coordinates with record
--         path            : type_path_to_submodule.list;
-- 		module_name		: type_submodule_name.bounded_string;
-- 		sheet_number	: positive;
-- 	end record;


	
	

	
-- TEXT FIELD

	-- A text/note field in the schematic gets extended by extended coordinates (see above)
	type type_note is new et_libraries.type_text_basic with record
		meaning			: et_libraries.type_text_meaning := et_libraries.note;
		coordinates		: et_coordinates.type_coordinates;
		content			: et_libraries.type_text_content.bounded_string;
	end record;

	procedure write_note_properties (note : in et_schematic.type_note);
	-- Writes the properties of the given note

	procedure add_note (
	-- Inserts a note in the the module (indicated by module_cursor).
		note	: in et_schematic.type_note);
	
	package type_texts is new indefinite_doubly_linked_lists (
		element_type => type_note);


-- UNITS AND COMPONENTS

	-- Units may have alternative representations such as de_Morgan
	type type_alternative_representation is ( NO, YES);

	-- units can be placed mirrored horizontal or vertical:
	type type_mirror is (none, horizontal, vertical);

	-- In a schematic we find units spread all over.
	-- A unit is a subsection of a component.
	-- A unit has placeholders for text like reference (like IC303), value (like 7400), ...
	-- Some placeholders are available when the component appears in both schematic and layout.
	type type_unit (appearance : type_component_appearance) is record
		position	: et_coordinates.type_coordinates;
		orientation	: et_libraries.type_angle;
		mirror		: type_mirror;
		timestamp	: et_string_processing.type_timestamp;
		name		: et_libraries.type_unit_name.bounded_string;
		alt_repres	: type_alternative_representation;
		reference	: et_libraries.type_text_placeholder (meaning => et_libraries.reference);
		value		: et_libraries.type_text_placeholder (meaning => et_libraries.value);
		commissioned: et_libraries.type_text_placeholder (meaning => et_libraries.commissioned);		
		updated		: et_libraries.type_text_placeholder (meaning => et_libraries.updated);		
		author		: et_libraries.type_text_placeholder (meaning => et_libraries.author);
		case appearance is
			when sch | pcb => null; -- CS
			when sch_pcb =>
				packge		: et_libraries.type_text_placeholder (meaning => et_libraries.packge); -- like "SOT23"
				datasheet	: et_libraries.type_text_placeholder (meaning => et_libraries.datasheet); -- might be useful for some special components
				purpose		: et_libraries.type_text_placeholder (meaning => et_libraries.purpose); -- to be filled in schematic later by the user
				partcode	: et_libraries.type_text_placeholder (meaning => et_libraries.partcode); -- like "R_PAC_S_0805_VAL_"
		end case;
		-- NOTE: The placeholders are defined in et_libraries. Thus they have only
		-- basic coordinates (x/y). Via the unit position the sheet and module
		-- name can be obtained.
	end record;

	
	-- Units of a component are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package type_units is new indefinite_ordered_maps (
		key_type => et_libraries.type_unit_name.bounded_string,
		"<" => et_libraries.type_unit_name."<",
		element_type => type_unit);

	-- After associating (by the operator) a schematic component with a package, the composite
	-- type_variant is set according to the desired package.
	type type_variant is record
		name	: et_libraries.type_component_variant_name.bounded_string;	-- the variant name like TL084N or TL084D
		variant	: et_libraries.type_component_variant; -- incorporates package, full library name, connection list
	end record;


	-- This is a component as it appears in the schematic.
	type type_component (appearance : type_component_appearance) is record
		name_in_library : et_libraries.type_component_name.bounded_string; -- example: "TRANSISTOR_PNP"
		value			: et_libraries.type_component_value.bounded_string; -- 470R
		commissioned	: et_string_processing.type_date; -- 2017-08-17T14:17:25
		updated			: et_string_processing.type_date; -- 2017-10-30T08:33:56
		author			: et_libraries.type_person_name.bounded_string; -- Steve Miller
		units			: type_units.map; -- PWR, A, B, ...
		case appearance is
			-- If a component appears in both schematic and layout it has got:
			when sch_pcb => 
				variant		: type_variant;
				partcode	: et_libraries.type_component_partcode.bounded_string;
				purpose		: et_libraries.type_component_purpose.bounded_string;
				datasheet	: et_libraries.type_component_datasheet.bounded_string;

			when sch => null;
			when others => null; -- CS
		end case;
	end record;

	procedure write_unit_properties (unit : in type_units.cursor);
	-- Writes the properties of the unit indicated by the given cursor.


-- LABELS AND NETS

	-- The name of a net may have 100 characters which seems sufficient for now.
 	net_name_length	: constant natural := 100;
	package type_net_name is new generic_bounded_length (net_name_length); use type_net_name;
	
	function to_string (net_name : in type_net_name.bounded_string) return string;
	-- Returns the given net name as string;

	-- A label is frequently attached to a net segment in order to make the net name visible. 
	-- The label can be a simple string or a tag. 
	-- The label carries the name of the net. 
	-- In case the appearance is "tag", the property "direction" exists.
	-- The "direction" indicates the flow of energy or information on the net.
	
	type type_label_direction is ( input, output, bidir, tristate, passive );
	type type_label_appearance is ( simple, tag );
	type type_net_label ( label_appearance : type_label_appearance ) is record
		coordinates	: et_coordinates.type_coordinates;
		orientation	: et_libraries.type_angle;
        text		: type_net_name.bounded_string;
        size		: et_libraries.type_text_size;
        style		: et_libraries.type_text_style;
        width		: et_libraries.type_text_line_width;
		processed	: boolean := false; -- used for associating label with net segment
		case label_appearance is
			when tag => 
				direction : type_label_direction;
				-- CS: coordinates of next tag of this net (by sheet coord. or area ?)
				global : boolean;
				hierarchic : boolean;
			when simple => null;
		end case;
	end record;

	-- The smallest element of a net is a segment. It has a start and an end point.
	-- It may have a list of simple labels. It may have a list of tag labels.
	type type_net_label_simple is new type_net_label (label_appearance => simple);
	package type_simple_labels is new doubly_linked_lists (
		element_type => type_net_label_simple);
	
	type type_net_label_tag is new type_net_label(label_appearance => tag);		
	package type_tag_labels is new doubly_linked_lists (
		element_type => type_net_label_tag);

	procedure write_label_properties (label : in type_net_label);
	-- Writes the properties of the given net label in the logfile.
    
	-- A net is something that carries electrical current between ports. It consists of one or more segments.

	-- A net junction is where segments can be connected with each other.
	type type_net_junction is tagged record
		coordinates : et_coordinates.type_coordinates;
	end record;

	procedure write_junction_properties (junction : in type_net_junction);
	-- Writes the properties of the given net junction in the logfile.
	
-- 	procedure write_coordinates_of_junction (junction : in type_net_junction); -- CS: no longer used
-- 	-- Writes the coordinates of a net junction.

	type type_net_segment; -- prespecificaton. see below
	
	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment'class) 
		-- NOTE: see https://en.wikibooks.org/wiki/Ada_Programming/Object_Orientation#Polymorphism.2C_class-wide_programming_and_dynamic_dispatching
		return boolean;
	
	-- Junctions are to be collected in a list.
	package type_junctions is new doubly_linked_lists (
		element_type => type_net_junction);

	
	-- A segment may have labels attached.
	-- So this is the definition of a net segment with start and end coord., lists of simple and tag labels
	type type_net_segment is tagged record
		coordinates_start 	: et_coordinates.type_coordinates;
		coordinates_end   	: et_coordinates.type_coordinates;
		--junctions			: type_junctions.list;
		label_list_simple 	: type_simple_labels.list;
		label_list_tag    	: type_tag_labels.list;
	end record;

	
	procedure write_coordinates_of_segment (segment : in type_net_segment);
	-- Writes the start and end coordinates of a net segment.	

	-- A net is a list of segments.
	package type_net_segments is new doubly_linked_lists (
		element_type => type_net_segment);

	-- A net may be visible within a submodule (local net) or 
	-- it may be exported to parent module (other ECAD tools refer to them as "hierachical or global nets").
	type type_scope_of_net is  ( local, hierarchic, global );

	-- This is a coponent port with its basic elements:
	type type_port_base is tagged record
		pin			: et_libraries.type_pin_name.bounded_string;
		port		: et_libraries.type_port_name.bounded_string;
		coordinates : et_coordinates.type_coordinates;
		direction	: type_port_direction; -- example: "passive" -- used for ERC
		style		: type_port_style;	-- used for ERC
	end record;


	-- If component ports are to be listed in a type_net, we need additionally the component reference:
	type type_port is new type_port_base with record
		reference	: et_libraries.type_component_reference;
	end record;

	function compare_ports (left, right : in type_port) return boolean;
	-- Returns true if left comes before right. Compares by component name and pin name.
	-- If left equals right, the return is false.	

	procedure add_port (
	-- Adds a port to a net in the current module (indicated by module_cursor).
		net		: in et_schematic.type_net_name.bounded_string;
		port	: in et_schematic.type_port);
	
	package type_ports is new ordered_sets (
		element_type => type_port,
		"<" => compare_ports);

	-- A net has a name, a scope, a list of segments, a list of ports.
    -- A net has coordinates
    -- CS: x/y position should be the lowest values available on the first sheet ? 
    -- CS: do not use sheet and x/y at all ?
    type type_net is record
		scope 		: type_scope_of_net; -- example "local"
		segments 	: type_net_segments.list; -- list of net segments
		--junctions	: type_junctions.list; -- the junctions of the net
        ports 		: type_ports.set; -- list of type_ports
		coordinates : et_coordinates.type_coordinates;
	end record;

	-- Nets are collected in a map.
	package type_nets is new ordered_maps (
		key_type => type_net_name.bounded_string, -- example "CPU_CLOCK"
		element_type => type_net);

	anonymous_net_name_prefix : constant string (1..2) := "N$";




	
	

-- VISUALISATION IN A GRAPHICAL USER INTERFACE
	-- How objects are displayed in a GUI.

    -- SUBMODULE
    -- A submodule is a box with coordinates and length x/y.
    -- On the box edges are ports.
    type type_gui_submodule is record -- CS: read from kicad $sheet
        text_size_of_name   : type_text_size;
        text_size_of_file   : type_text_size;        
		coordinates		    : type_coordinates;
        size_x, size_y      : type_distance; -- size x/y of the box
        timestamp           : et_string_processing.type_timestamp;
        -- CS: ports ?
	end record;

    -- A list of submodules is a component of the main module:    
    package type_gui_submodules is new ordered_maps (
        key_type => et_coordinates.type_submodule_name.bounded_string,
		"<" => et_coordinates.type_submodule_name."<",
        element_type => type_gui_submodule);


    -- DRAWING FRAME
    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is record
		coordinates_start : type_2d_point;
        coordinates_end   : type_2d_point;
	end record;
	
	package type_frame_lines is new doubly_linked_lists (
        element_type => type_frame_line);

	type type_frame_text is record
		coordinates		: type_2d_point;
		text			: character_set := et_string_processing.general_characters;
		size			: type_text_size;
		orientation		: type_angle;
		-- CS: font, ...
	end record;
	
	package type_frame_texts is new doubly_linked_lists (
        element_type => type_frame_text);

    -- the final drawing frame
    type type_frame is record
        coordinates     : type_coordinates; -- the position of the frame
        paper_size      : type_paper_size; -- the size of the paper
        size_x, size_y  : type_distance; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_frame_lines.list;
        texts           : type_frame_texts.list;
    end record;

    -- there are lots of drawing frames in a schematic contained in a list
    package type_frames is new doubly_linked_lists (
        element_type => type_frame);

    -- TITLE BLOCK
    type type_title_block_line is record
		coordinates_start : type_2d_point;
		coordinates_end   : type_2d_point;
    end record;

	package type_title_block_lines is new doubly_linked_lists (
        element_type => type_title_block_line);
    
 	title_block_text_length	: constant natural := 200;
	package type_title_block_text_string is new generic_bounded_length(title_block_text_length); use type_title_block_text_string;

	type type_title_block_text_meaning is ( PROJECT, TITLE, 
        DRAWN_BY, CHECKED_BY, APPROVED_BY, 
        DRAWN_DATE, CHECKED_DATE, APPROVED_DATE,
        COMPANY,
		REVISION, MISC);
	
	type type_title_block_text is record -- CS: from kicad $descr
		meaning			: type_title_block_text_meaning;
 		coordinates		: type_2d_point;
		text			: type_title_block_text_string.bounded_string;
 		size			: type_text_size;
 		orientation		: type_angle;
		-- CS: font, ...
 	end record;

	package type_title_block_texts is new doubly_linked_lists (
 		element_type => type_title_block_text);

    -- the final title block
    type type_title_block is record
        coordinates     : et_coordinates.type_coordinates;
        lines           : type_title_block_lines.list;
        texts           : type_title_block_texts.list;
    end record;

    -- there are lots of title blocks in a schematic contained in a list
    package type_title_blocks is new doubly_linked_lists (
        element_type => type_title_block);

	-- sheet headers (kicad requirement)
	-- The sheet header is a composite of a list of libraries and other things:
	-- It contains a list of libraries used by a schemetic sheet.
	-- We use a simple list because the order of the library names must be kept.
    type type_sheet_header is record
        version     : positive; -- 2    
		libraries   : et_libraries.type_library_names.list; -- CS: probably not used by kicad, just information
		--libraries	: type_list_of_library_names.map;
        eelayer_a   : positive; -- 25 -- CS: meaning not clear, probably not used
        eelayer_b   : natural; -- 0 -- CS: meaning not clear, probably not used
    end record;

	-- Since there are usually many sheets, we need a map from schematic file name to schematic header.
    package type_sheet_headers is new ordered_maps (
        key_type => type_schematic_file_name.bounded_string,
        element_type => type_sheet_header);
	
	
    
-- 	type type_gui_sheet is record -- CS: from kicad $descr
--         title           : type_sheet_title.bounded_string;
--         project         : type_project_name.bounded_string;
--         checked_by      : type_person_name.bounded_string;
--         checked_date    : string (1..date_field_length); -- CS: use a well definded date type
--         approved_by     : type_person_name.bounded_string;
--         approved_date   : string (1..date_field_length);
--         drawn_by        : type_person_name.bounded_string;
--         drawn_date      : string (1..date_field_length);
--         size_x, size_y  : type_grid; -- size x/y of the sheet
--         comment         : type_sheet_comment.bounded_string; -- CS: more of them in a list
--         -- CS: list of texts
--         -- CS border/frame type
-- 	end record;    




	
	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_charater_in_prefix is given true, the first character
	-- is allowed to be a special character. Example: "L P3V3 #PWR07"
	-- NOTE: Leading zeroes in the id are removed.	
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false)
		return type_component_reference;

	function to_string (reference : in type_component_reference) return string;
	-- Returns the given compoenent reference as string.

	function compare_reference (left, right : in type_component_reference) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	
	
	function equal_reference (left, right : in type_component_reference) return boolean;
	-- Returns true if left equals right.




    


-- MODULE

	-- The components of a module are collected in a map.
 	package type_components is new indefinite_ordered_maps (
		key_type => type_component_reference, -- something like "IC43"
		"<" => compare_reference,
 		element_type => type_component);

	procedure write_component_properties (component : in type_components.cursor);
	-- Writes the properties of the component indicated by the given cursor.

	function component_reference (cursor : in type_components.cursor) 
		return type_component_reference;
	-- Returns the component reference where cursor points to.

	function component_name_in_library (cursor : in type_components.cursor) 
		return et_libraries.type_component_name.bounded_string;
	-- Returns the generic name of a component as it is listed in a library.
	-- The cursor must point to the component in question.


-- PORTLISTS	
	-- Base ports of a component are collected in a simple list.
	package type_base_ports is new doubly_linked_lists ( 
		element_type => type_port_base); 
	use type_base_ports;

	-- The components with their ports are collected in a map with the component reference as key:
	package type_portlists is new ordered_maps (
		key_type => et_libraries.type_component_reference,
		element_type => type_base_ports.list,
		"<" => et_schematic.compare_reference);

	function build_portlists return type_portlists.map;
	-- Returns a list of components with the absolute positions of their ports.
	

	
-- MODULES
	
    -- A module has a name, a list of nets and a list of components.
    -- Objects relevant for graphical interfaces are
    -- - a list of submodules
    -- - a list of drawing frames
    -- - a list of title blocks
	type type_module is record
		libraries		: type_full_library_names.list;	-- the list of project library names
		nets 	    	: type_nets.map;			-- the nets of the module
		portlists		: type_portlists.map;		-- the portlists of the module
        components		: type_components.map;		-- the components of the module
		submodules  	: type_gui_submodules.map;	-- graphical representations of submodules
        frames      	: type_frames.list;			-- frames
        title_blocks	: type_title_blocks.list;	-- title blocks
		notes       	: type_texts.list;			-- notes

		sheet_headers	: type_sheet_headers.map;	-- the list of sheet headers -- kicad requirement
		-- CS: junctions
        -- CS: images
	end record;


	-- A rig is a set of modules:
	package type_rig is new ordered_maps (
		key_type => et_coordinates.type_submodule_name.bounded_string, -- example "MOTOR_DRIVER"
		"<" => et_coordinates.type_submodule_name."<",											 
		element_type => type_module);

	rig : type_rig.map;
	module_cursor : type_rig.cursor;


	procedure add_module (
	-- Adds a module into the rig. Leaves module_cursor pointing
	-- to the module inserted last.
		module_name : in et_coordinates.type_submodule_name.bounded_string;
		module		: in type_module);

	procedure add_gui_submodule (
	-- Inserts a gui submodule in the module (indicated by module_cursor)
		name		: in et_coordinates.type_submodule_name.bounded_string;
		gui_sub_mod	: in et_schematic.type_gui_submodule);

	procedure add_sheet_header (
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string);
	
	procedure add_frame (
	-- Inserts a drawing frame in the module (indicated by module_cursor).
		frame	: in et_schematic.type_frame);
	
	procedure add_title_block (
	-- Inserts a title block in the module (indicated by module_cursor).
		tblock	: in et_schematic.type_title_block);

	procedure add_net (
	-- Adds a net into the module (indicated by module_cursor).
		name	: in et_schematic.type_net_name.bounded_string;
		net		: in et_schematic.type_net);

	procedure add_portlists (
	-- Adds the portlists into the module (indicated by module.cursor)
		portlists : in et_schematic.type_portlists.map);
	
	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
		reference	: in et_libraries.type_component_reference;
		component	: in type_component);

	procedure add_unit (
	-- Adds a unit into the given commponent.
		reference	: in et_libraries.type_component_reference;
		unit_name	: in et_libraries.type_unit_name.bounded_string;
		unit 		: in type_unit);

	procedure reset_component_cursor (cursor : in out type_components.cursor);
	-- Resets the given component cursor to the begin of the component list.

	procedure reset_library_cursor (cursor : in out type_full_library_names.cursor);
	-- Resets the given library cursor to the begin of the library list.

	function number_of_libraries return count_type;
	-- Returns the number of project libraries.
	
-- 	function get_component_reference (cursor : in out type_components.cursor) 
-- 	-- Returns the component reference where the component cursor points to.
-- 		return type_component_reference;

	function units_of_component (component_cursor : in type_components.cursor) return type_units.map;
	-- Returns the units of the given component.

-- 	function position_of_unit (
-- 	-- Returns the coordinates of the given unit.
-- 	-- The unit is one element in the given list of units.
-- 		name : in type_unit_name.bounded_string; -- the unit being inquired
-- 		units : in type_units.map) -- the list of units
-- 		return type_coordinates;
	
	procedure warning_on_name_less_net (
	-- Writes a warning about a name-less net.
		name 	: in et_schematic.type_net_name.bounded_string;
		net		: in et_schematic.type_net);

	
    
-- MISC

    -- When reading a schematic sheet, submodules might be discovered.
    -- They are returned to the parent unit in a list of submodules:
	package type_submodule_names is new vectors ( -- the bare list -- CS: better an ordered set ?
		index_type => positive,
		"=" => et_coordinates.type_submodule_name."=",
		element_type => et_coordinates.type_submodule_name.bounded_string);

    -- A composite type with additional supporting information:
	type type_submodule_names_extended is record
		parent_module	: et_coordinates.type_submodule_name.bounded_string;
		list			: type_submodule_names.vector;
		id				: positive; -- id of a submodule in the list
	end record;




	
	
end et_schematic;

-- Soli Deo Gloria
