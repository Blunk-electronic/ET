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

with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with et_general;                use et_general;
with et_libraries;

package et_schematic is

	procedure a; -- CS: dummy, remove it !



-- NAMES GENERAL

	-- The name of a net may have 100 characters which seems sufficient for now.
 	net_name_length	: constant natural := 100;
	package type_net_name is new generic_bounded_length(net_name_length); use type_net_name;

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

   
-- TIMESTAMP
    -- CS: type_timestamp 
    
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
	
	type type_coordinates is new et_general.type_coordinates with record
        path            : type_path_to_submodule.list;
		module_name		: type_submodule_name.bounded_string;
		sheet_number	: positive;
	end record;


-- TEXT FIELD

	type type_text_field is new et_general.type_text_field with record
		coordinates             : type_coordinates;
	end record;
	package type_text_fields is new doubly_linked_lists (
		element_type => type_text_field);

	
	
-- DEVICE
	
	type type_unit is record
		name			: et_libraries.type_unit_name.bounded_string;
		position		: type_coordinates;
		fields			: type_text_fields.list;	
	end record;

	-- Units of a device will be collected in a map.
	package type_units is new ordered_maps (
		key_type => et_libraries.type_unit_name.bounded_string,
		"<" => et_libraries.type_unit_name."<",
		element_type => type_unit);
	
	type type_device is new et_general.type_component with record
		id				: positive; -- together with the prefix we get something like "IC702"
		name_in_library : et_libraries.type_component_name.bounded_string; -- example: "TRANSISTOR_PNP"
		units			: type_units.map;
	end record;









	

-- LABEL
	-- A label is frequently attached to a net segment in order to make the net name visible. 
	-- The label can be a simple string or a tag. 
	-- The label carries the name of the net. 
	-- In case the appearance is "tag", the property "direction" exists.
	-- The "direction" indicates the flow of energy or information on the net.
	
	type type_label_direction is ( input, output, bidir, tristate, passive );
	type type_label_appearance is ( simple, tag );
	type type_net_label ( label_appearance : type_label_appearance ) is record
		coordinates       : type_coordinates;
		orientation       : type_orientation;
        text              : type_net_name.bounded_string;
        text_attributes   : type_text_attributes;
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


    

-- NET
	-- A net is something that carries electrical current between ports. It consists of one or more segments.
	-- A segment may have labels attached.
    
	-- So this is the definition of a net segment with start and end coord., lists of simple and tag labels
	type type_net_segment is tagged record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		label_list_simple : type_list_of_labels_simple.vector;
		label_list_tag    : type_list_of_labels_tag.vector;
	end record;

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
		element_type => et_libraries.type_port);

	-- A net junction is where segments can be connected with each other.
	type type_net_junction is tagged record
		coordinates : type_coordinates;
	end record;

	-- Junctions are to be collected in a list.
	package type_list_of_net_junctions is new vectors (
		index_type => positive, -- every junction has an id
		element_type => type_net_junction);
		
    -- A net has a name, a scope, a list of segments, a list of ports.
    -- A net has coordinates
    -- CS: x/y position should be the lowest values available on the first sheet ? 
    -- CS: do not use sheet and x/y at all ?
    type type_net is record
		name 		: type_net_name.bounded_string; -- example "CPU_CLOCK"
		scope 		: type_scope_of_net; -- example "local"
		segments 	: type_list_of_net_segments.vector; -- list of net segments
        ports 		: type_port_list_of_net.vector; -- list of device ports
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
        size_x, size_y      : et_general.type_grid; -- size x/y of the box
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
		coordinates_start : et_general.type_coordinates;
        coordinates_end   : et_general.type_coordinates;
    end record;
	package type_list_of_frame_lines is new vectors (
		index_type => positive, -- every line of a frame an id
        element_type => type_frame_line);
    
	type type_frame_text is record
		coordinates		: et_general.type_coordinates;
		text			: character; -- CS: range A..Z and 1..n
		size			: type_text_size;
		orientation		: type_text_orientation;
		-- CS: font, ...
	end record;
	package type_list_of_frame_texts is new vectors (
		index_type => positive, -- every text field a frame has an id
        element_type => type_frame_text);

    -- the final drawing frame
    type type_frame is record
        coordinates     : type_coordinates;
        paper_size      : type_paper_size; -- the size of the paper
        size_x, size_y  : et_general.type_grid; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_list_of_frame_lines.vector;
        texts           : type_list_of_frame_texts.vector;
    end record;

    -- there are lots of drawing frames in a schematic contained in a list
    package type_list_of_frames is new vectors (
        index_type => positive, -- every drawing fram has an id
        element_type => type_frame);

    -- TITLE BLOCK
    type type_title_block_line is record
		coordinates_start : et_general.type_coordinates;
		coordinates_end   : et_general.type_coordinates;
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
 		coordinates		: et_general.type_coordinates;
		text			: type_title_block_text_string.bounded_string;
 		size			: type_text_size;
 		orientation		: type_text_orientation;
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








    


-- MODULE
	-- The nets of a module are collected in a vector list.
	package type_net_list_of_module is new vectors (
		index_type => positive, -- every net of a module has an id
		element_type => type_net);

	-- The devices of a module are collected in a map.
	use et_general.type_device_name;
 	package type_device_list_of_module is new ordered_maps (
 		key_type => et_general.type_device_name.bounded_string, -- something like "IC43"
 		element_type => type_device);

	-- CS: could be of interest when a composite type for device names is used. see above.
-- 	package type_device_list_of_module2 is new ordered_maps (
--  		key_type => type_device_name2, -- something like "IC43"
--  		element_type => type_device);

	
    -- A module has a name, a list of nets and a list of devices.
    -- Objects relevant for graphical interfaces are
    -- - a list of submodules
    -- - a list of drawing frames
    -- - a list of title blocks
	type type_module is record
		name 	    : type_submodule_name.bounded_string; -- example "MOTOR_DRIVER"
		nets 	    : type_net_list_of_module.vector;
        devices     : type_device_list_of_module.map;
        submodules  : type_list_of_gui_submodules.vector;
        frames      : type_list_of_frames.vector;
        title_blocks: type_list_of_title_blocks.vector;
        notes       : type_text_fields.list;
        -- CS: images
	end record;

	module : type_module; -- this is the whole schematic of a board
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

