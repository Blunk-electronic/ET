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

with et_general;
with et_libraries;				use et_libraries;
with et_string_processing;

package et_schematic is

-- NAMES GENERAL

	-- The name of a module may have 100 characters which seems sufficient for now.
 	module_name_length	: constant natural := 100;
	package type_module_name is new generic_bounded_length(module_name_length); use type_module_name;

	-- The name of a submodule may have 100 characters which seems sufficient for now.
 	submodule_name_length : constant natural := 100;
	package type_submodule_name is new generic_bounded_length(submodule_name_length); use type_submodule_name;

    -- A sheet title may have 100 characters which seems sufficient for now.
 	sheet_title_length	: constant natural := 100;    
	package type_sheet_title is new generic_bounded_length(sheet_title_length); use type_sheet_title;
    
    sheet_comment_length : constant natural := 100;
    package type_sheet_comment is new generic_bounded_length(sheet_comment_length); use type_sheet_comment;

   
    
-- COORDINATES
   
	-- Within a schematic every object can be located by the name of the:
    -- - path to the submodule (first item in path is the top level module)
	-- - submodule name
	-- - sheet number (NOTE: The sheet numbering restarts in a submodule)
	-- - basic coordinates x/y

    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver.counter.supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);
	
	type type_coordinates is new et_libraries.type_coordinates with record
        path            : type_path_to_submodule.list;
		module_name		: type_submodule_name.bounded_string;
		sheet_number	: positive;
	end record;

	coordinates_preamble : constant string (1..21) := "position " 
		& "(sheet"
		& et_libraries.coordinates_dimension_separator
		& "x"
		& et_libraries.coordinates_dimension_separator
		& "y) ";
	
	function to_string (position : in type_coordinates) return string;
	-- Returns the given position as string.

	
-- TEXT FIELD

	-- A text/note field in the schematic gets extended by extended coordinates (see above)
	type type_note is new et_libraries.type_text_basic with record
		meaning			: et_libraries.type_text_meaning := et_libraries.note;
		coordinates		: type_coordinates;
		content			: et_libraries.type_text_content.bounded_string;
	end record;

	procedure write_note_properties (note : in et_schematic.type_note);
	-- Writes the properties of the given note
	
	package type_texts is new indefinite_doubly_linked_lists (
		element_type => type_note);


-- UNITS AND COMPONENTS

	-- Units may have alternative representations such as de_Morgan
	type type_alternative_representation is ( NO, YES);

	-- In a schematic we find units spread all over.
	-- A unit is a subsection of a component.
	-- A unit has placeholders for text like reference (like IC303), value (like 7400), ...
	-- Some placeholders are available when the component appears in both schematic and layout.
	type type_unit (appearance : type_component_appearance) is record
		position	: type_coordinates;
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


-- NETS
	-- The name of a net may have 100 characters which seems sufficient for now.
 	net_name_length	: constant natural := 100;
	package type_net_name is new generic_bounded_length(net_name_length); use type_net_name;

	

-- LABEL
	-- A label is frequently attached to a net segment in order to make the net name visible. 
	-- The label can be a simple string or a tag. 
	-- The label carries the name of the net. 
	-- In case the appearance is "tag", the property "direction" exists.
	-- The "direction" indicates the flow of energy or information on the net.
	
	type type_label_direction is ( input, output, bidir, tristate, passive );
	type type_label_appearance is ( simple, tag );
	type type_net_label ( label_appearance : type_label_appearance ) is record
		coordinates	: type_coordinates;
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
	type type_net_label_simple is new type_net_label(label_appearance => simple);
	package type_list_of_labels_simple is new vectors ( 
		index_type => positive, -- every simple label has an id
		element_type => type_net_label_simple);
	
	type type_net_label_tag is new type_net_label(label_appearance => tag);		
	package type_list_of_labels_tag is new vectors ( 
		index_type => positive, -- every tag label has an id
		element_type => type_net_label_tag);

	procedure write_label_properties ( label : in type_net_label);
	-- Writes the properties of the given net label in the logfile.
    

-- NET
	-- A net is something that carries electrical current between ports. It consists of one or more segments.
	-- A segment may have labels attached.
    
	-- So this is the definition of a net segment with start and end coord., lists of simple and tag labels
	type type_net_segment is tagged record
		coordinates_start : type_coordinates; -- CS: better et_libraries.type_coordinates ?
		coordinates_end   : type_coordinates;
		label_list_simple : type_list_of_labels_simple.vector;
		label_list_tag    : type_list_of_labels_tag.vector;
	end record;

	procedure write_coordinates_of_segment (segment : in type_net_segment);
	-- Writes the start and end coordinates of a net segment.	
	
	-- A net is a list of segments.
	package type_list_of_net_segments is new vectors ( 
		index_type => positive,  -- every net segment has an id
		element_type => type_net_segment);

	-- A net may be visible within a submodule (local net) or 
	-- it may be exported to parent module (other ECAD tools refer to them as "hierachical or global nets").
	type type_scope_of_net is  ( local, hierarchic, global );

	-- A list of type_port forms a port list of a net.
	use et_libraries;
	package type_port_list_of_net is new vectors ( 
		index_type => positive, -- every pin of a net has an id
		element_type => et_libraries.type_port); -- CS: this should be a schematic specific type_port

	-- A net junction is where segments can be connected with each other.
	type type_net_junction is tagged record
		coordinates : type_coordinates;
	end record;

	procedure write_coordinates_of_junction (junction : in type_net_junction);
	-- Writes the coordinates of a net junction.
	
	-- Junctions are to be collected in a list.
	package type_list_of_net_junctions is new vectors (
		index_type => positive, -- every junction has an id
		element_type => type_net_junction);
		
	-- A net has a name, a scope, a list of segments, a list of ports.
	anonymous_net_name_prefix : constant string (1..2) := "N$";
	
    -- A net has coordinates
    -- CS: x/y position should be the lowest values available on the first sheet ? 
    -- CS: do not use sheet and x/y at all ?
    type type_net is record
		name 		: type_net_name.bounded_string; -- example "CPU_CLOCK"
		scope 		: type_scope_of_net; -- example "local"
		segments 	: type_list_of_net_segments.vector; -- list of net segments
		-- CS: junctions ? 
        ports 		: type_port_list_of_net.vector; -- list of component ports
		coordinates : type_coordinates;                
	end record;









	

-- VISUALISATION IN A GRAPHICAL USER INTERFACE
	-- How objects are displayed in a GUI.

    -- SUBMODULE
    -- A submodule is a box with coordinates and length x/y.
    -- On the box edges are ports.
    type type_gui_submodule is record -- CS: read from kicad $sheet
        name                : type_submodule_name.bounded_string;
        text_size_of_name   : type_text_size;
        text_size_of_file   : type_text_size;        
		coordinates		    : type_coordinates;
        size_x, size_y      : et_libraries.type_grid; -- size x/y of the box
        timestamp           : string (1..8); -- CS: type_timestamp
        -- CS: ports ?
	end record;

    -- A list of submodules is a component of the main module:    
    package type_list_of_gui_submodules is new vectors (
        index_type => positive, -- every gui submodule has an id
        element_type => type_gui_submodule);



    -- DRAWING FRAME
    -- A drawing frame consists of straight lines and texts.
    -- The text is a character at the x/y border that helps to locate objects.
    type type_frame_line is record
		coordinates_start : et_libraries.type_coordinates;
        coordinates_end   : et_libraries.type_coordinates;
    end record;
	package type_list_of_frame_lines is new vectors (
		index_type => positive, -- every line of a frame an id
        element_type => type_frame_line);
    
	type type_frame_text is record
		coordinates		: et_libraries.type_coordinates;
		text			: character; -- CS: range A..Z and 1..n
		size			: type_text_size;
		orientation		: type_angle;
		-- CS: font, ...
	end record;
	package type_list_of_frame_texts is new vectors (
		index_type => positive, -- every text field a frame has an id
        element_type => type_frame_text);

    -- the final drawing frame
    type type_frame is record
        coordinates     : type_coordinates;
        paper_size      : et_general.type_paper_size; -- the size of the paper
        size_x, size_y  : et_libraries.type_grid; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_list_of_frame_lines.vector;
        texts           : type_list_of_frame_texts.vector;
    end record;

    -- there are lots of drawing frames in a schematic contained in a list
    package type_list_of_frames is new vectors (
        index_type => positive, -- every drawing fram has an id
        element_type => type_frame);

    -- TITLE BLOCK
    type type_title_block_line is record
		coordinates_start : et_libraries.type_coordinates;
		coordinates_end   : et_libraries.type_coordinates;
    end record;
	package type_list_of_title_block_lines is new vectors (
		index_type => positive, -- every line of a frame an id
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
 		coordinates		: et_libraries.type_coordinates;
		text			: type_title_block_text_string.bounded_string;
 		size			: type_text_size;
 		orientation		: type_angle;
		-- CS: font, ...
 	end record;
 	package type_list_of_title_block_texts is new vectors (
 		index_type => positive, -- every text field of the title block has an id
 		element_type => type_title_block_text);

    -- the final title block
    type type_title_block is record
        coordinates     : type_coordinates;
        lines           : type_list_of_title_block_lines.vector;
        texts           : type_list_of_title_block_texts.vector;
    end record;

    -- there are lots of title blocks in a schematic contained in a list
    package type_list_of_title_blocks is new vectors (
        index_type => positive, -- every title block has an id
        element_type => type_title_block);

    
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



	type type_component_reference is record -- CS: should be private
		prefix		: type_component_prefix.bounded_string := component_reference_prefix_default; -- like "IC"
		id			: natural := component_reference_id_default; -- like "303"
		id_width	: positive; -- the number of digits in the id. 3 in case of an id of 303
		-- NOTE: This allows something like R091 or IC0 (there are reasons for such strange things ...)
	end record;

	function to_string (value : in type_component_value.bounded_string) return string;
	-- Returns the given value as string.
	
	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference) 
		return boolean;
	
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

	function compare_component_by_reference (left, right : in type_component_reference) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	
	




    


-- MODULE
	-- The nets of a module are collected in a vector list.
	package type_net_list_of_module is new vectors ( -- CS: should be map with net name as key
		index_type => positive, -- every net of a module has an id
		element_type => type_net);

	-- The components of a module are collected in a map.
 	package type_components is new indefinite_ordered_maps (
		key_type => type_component_reference, -- something like "IC43"
		"<" => compare_component_by_reference,
 		element_type => type_component);

	procedure write_component_properties ( component : in type_components.cursor);
	-- Writes the properties of the component indicated by the given cursor.

	
    -- A module has a name, a list of nets and a list of components.
    -- Objects relevant for graphical interfaces are
    -- - a list of submodules
    -- - a list of drawing frames
    -- - a list of title blocks
	type type_module is record
		name 	    : type_submodule_name.bounded_string; -- example "MOTOR_DRIVER"
		nets 	    : type_net_list_of_module.vector;
        components	: type_components.map;
        submodules  : type_list_of_gui_submodules.vector;
        frames      : type_list_of_frames.vector;
        title_blocks: type_list_of_title_blocks.vector;
        notes       : type_texts.list;
        -- CS: images
	end record;

	module : type_module; -- this is the whole schematic of a board

	-- For multi-board support:
    -- CS: list of modules







    
-- MISC

    -- When reading a schematic sheet, submodules might be discovered.
    -- They are returned to the parent unit in a list of submodules:
	package type_list_of_submodule_names is new vectors ( -- the bare list
		index_type => positive,
		element_type => type_submodule_name.bounded_string);

    -- A composite type with additional supporting information:
	type type_list_of_submodule_names_extended is record
		parent_module	: type_submodule_name.bounded_string;
		list			: type_list_of_submodule_names.vector;
		id				: positive; -- id of a submodule in the list
	end record;



	
	
end et_schematic;

-- Soli Deo Gloria
