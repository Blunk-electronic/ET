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

with et_general;                use et_general;
package et_schematic is

	procedure a; -- CS: dummy, remove it !



-- NAMES GENERAL
	-- The name of a device may have 100 characters which seems sufficient for now.
 	device_name_length	: constant natural := 100;
	package type_device_name is new generic_bounded_length(device_name_length); use type_device_name;

 	port_name_length	: constant natural := 50;
	package type_port_name is new generic_bounded_length(port_name_length); use type_port_name;

	device_block_name_length : constant natural := 50;
	package type_device_block_name is new generic_bounded_length(device_block_name_length); use type_device_block_name;

 	device_name_in_library_length : constant natural := 100;
	package type_device_name_in_library is new generic_bounded_length(device_name_in_library_length); use type_device_name_in_library;
	
	-- The name of a pin may have 10 characters which seems sufficient for now.
 	pin_name_length	: constant natural := 10;
	package type_pin_name is new generic_bounded_length(pin_name_length); use type_pin_name;

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

    library_name_length : constant natural := 100;
    package type_library_name is new generic_bounded_length(library_name_length); use type_library_name;
    
-- TIMESTAMP
    -- CS: type_timestamp 
    
-- COORDINATES
	-- Initially a library level, there is only x an y coordinates. Later the name of the submodule
	-- and the sheet are assigned. CS: set defaults
	--type type_grid is new natural; -- CS: needs further refinement
	type type_grid_extended is digits 11 range -100000000.00 .. 100000000.00;	
	subtype type_grid is type_grid_extended range -100000.00 .. 100000.00; -- CS: unit assumed is MIL !!!
	-- CS: negative schematic coordinates should be forbidden
	-- type type_grid is digits 7 range 0.00 .. 100000.00; -- CS: unit assumed is MIL !!!	

    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver.counter.supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);
    
    -- In general every object has x,y coordinates. Further components will
    -- extend this type later.
	type type_coordinates_basic is tagged record
		x,y				: type_grid;
		-- CS: layer ?		
	end record;
   
	-- Within a schematic every object can be located by the name of the:
    -- - path to the submodule (first item in path is the top level module)
	-- - submodule name
	-- - sheet number (NOTE: The sheet numbering restarts in a submodule)
	-- - basic coordinates x/y
	type type_coordinates is new type_coordinates_basic with record
        path            : type_path_to_submodule.list;
		module_name		: type_submodule_name.bounded_string;
		sheet_number	: positive;
	end record;

	
-- ORIENTATION	
	-- Objects may be placed at a certain angle:
	type type_orientation is ( deg_0, deg_90, deg_180, deg_270); -- other angles are not reasonable

-- TEXTS
    -- CS: currently we use unit mil which is old fashionated
    type type_text_size is range 1..1000; -- CS unit assumed is MIL !!!
	type type_text_line_width is range 0..100; -- CS unit assumed is MIL !!!
    type type_text_style is ( default, italic, bold, bold_italic);
    type type_text_attributes is record
        --font    : type_text_font; -- CS
        size    : type_text_size;
        style   : type_text_style;
        width   : type_text_line_width;
    end record;

    -- Texts may be placed at 0 or 90 degree only.
    subtype type_text_orientation is type_orientation range deg_0..deg_90; 

    type type_text_alignment_horizontal is ( left, center , right);
    type type_text_alignment_vertical is ( top, center , bottom);    


-- NOTES
    type type_note is record
        coordinates     : type_coordinates;    
        orientation     : type_orientation;        
        text            : unbounded_string;
        text_attributes : type_text_attributes;
    end record;
	package type_list_of_notes is new vectors (
		index_type => positive, -- every note has an id
		element_type => type_note);
    

-- PORT
	-- A port is something where a net can be attached at.
	-- The name of a port represents the function of the port like (A14 or RST_N)

	-- The port has an electrical direction:
	type type_port_direction is (
		DIGIAL_IN,
		DIGIAL_OUT,
		ANALOG_IN,
		ANALOG_OUT,
		PASSIVE, 		-- no explicit direction
		NOT_CONNECTED,
		POWER_OUT, 		-- a power source
		POWER_IN		-- a power sink
		);

	-- Initially, at the lowest level (usually library level), a port has a name, direction,
	-- coordinates, orientation, flags for making port and pin name visible. 
	-- Later, other values are assigned like pin name and device. CS: set defaults
	type type_port is record
		name              : type_port_name.bounded_string; -- example: "CLOCK"
		direction         : type_port_direction; -- example: "passive"
		coordinates       : type_coordinates;
		orientation       : type_orientation;
		display_port_name : boolean := true;
		display_pin_name  : boolean := true;
		pin               : type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"
 		device            : type_device_name.bounded_string; -- example: "IC501"		
	end record;


-- DEVICE
	
	-- BLOCK
	-- A device block is a sub-unit of a schematic device. Ohter CAE tools refer to them as "gate".
	-- A schematic device contains at least one block.
	-- Examples of a block: resistor symbol, i/o-bank of an fpga, power supply of an op-amp

	-- outline segments 
	-- The device block outline is composed of various elements like lines, arcs or cicles.
	
	-- Straight lines of a block will be collected in a vector list.
	type type_device_block_outline_segment_line is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
	end record;
	package type_list_of_device_block_outline_segments_line is new vectors (
		index_type => positive, -- every segment of a block outline has an id
		element_type => type_device_block_outline_segment_line);

	-- Arcs of a block will be collected in a vector list.
	type type_device_block_outline_segment_arc is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		coordinates_circumfence : type_coordinates;
	end record;
	package type_list_of_device_block_outline_segments_arcs is new vectors (
		index_type => positive, -- every segment of a block outline has an id
		element_type => type_device_block_outline_segment_arc);

	-- Circles of a block will be collected in a vector list.
	type type_device_block_outline_segment_circle is record
		coordinates_start : type_coordinates;
		coordinates_end   : type_coordinates;
		coordinates_center: type_coordinates;
	end record;
	package type_list_of_device_block_outline_segments_circles is new vectors (
		index_type => positive, -- every segment of a block outline has an id
		element_type => type_device_block_outline_segment_circle);

	-- Text fields of a block will be collected in a vector list.
	-- Text fields (usually inside) a device block give more information on what the block is supposed for.
	-- Example: BANK_1 , BANK_2, PWR_SUPPLY, CT10, CT2, MCU
	-- A text field of a block may have 100 characters which seems sufficient for now.
 	device_block_text_length	: constant natural := 200;
	package type_device_block_text_string is new generic_bounded_length(device_block_text_length); use type_device_block_text_string;
	type type_block_text_meaning is ( ANNOTATION, VALUE, FOOTPRINT, MISC); -- CS: partcode, function, ...
	type type_device_block_text is record
		meaning			        : type_block_text_meaning;
		coordinates             : type_coordinates;
        text                    : type_device_block_text_string.bounded_string;
        text_attributes         : type_text_attributes;
        orientation             : type_text_orientation;
        visible                 : boolean;
        alignment_horizontal    : type_text_alignment_horizontal;
        alignment_vertical      : type_text_alignment_vertical;        
	end record;
	package type_list_of_device_block_texts is new vectors (
		index_type => positive, -- every text field of a block outline has an id
		element_type => type_device_block_text);

	-- Ports of a block will be collected in a vector list.
	package type_list_of_device_block_ports is new vectors ( 
		index_type => positive, -- every port of a device block has an id
		element_type => type_port);

	-- A block has a name, coordinates, consists of segment lists , ports and texts.
	type type_device_block is record
		name					: type_device_block_name.bounded_string;
		coordinates				: type_coordinates;
		outline_segments_line 	: type_list_of_device_block_outline_segments_line.vector;
		outline_segments_arcs 	: type_list_of_device_block_outline_segments_arcs.vector;
		outline_segments_circles: type_list_of_device_block_outline_segments_circles.vector;
		port_list 				: type_list_of_device_block_ports.vector;
        text_list				: type_list_of_device_block_texts.vector;
        -- CS: timestamp
	end record;

	-- Blocks of a device will be collected in a vector list.
	package type_device_block_list is new vectors (
		index_type => positive, -- every block has an id
		element_type => type_device_block);

	-- DEVICE	
	-- A device has a physical appearance, a generic name in the library, an annotation in the schematic,
	-- a list of blocks, ...
	type type_device_physical_appearance is ( virtual, footprint); -- CS: cable , wire ?
	type type_device is record
		physical_appearance : type_device_physical_appearance := virtual; -- sometimes there is just a schematic
		name_in_library 	: type_device_name_in_library.bounded_string; -- example: "TRANSISTOR_PNP"
		annotation			: type_device_name.bounded_string; -- CS: includes prefix (R,C,L, ..) and number later given. example: "R501"
		-- CS: library file name ?
		block_list 			: type_device_block_list.vector;
-- 		case physical_appearance is
-- 			when footprint =>
-- 				null; 		-- CS: port-pin map ?
-- 			when others => 
-- 				null;
		-- 		end case;

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
	package type_port_list_of_net is new vectors ( 
		index_type => positive, -- every pin of a net has an id
		element_type => type_port);

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





-- LIBRARIES
    package type_list_of_library_names is new vectors (
        index_type => positive, -- every library name has an id
        element_type => type_library_name.bounded_string);




	

-- VISUALISATION IN A GRAPHICAL USER INTERFACE
	-- How objects are displayed in a GUI.

    -- SUBMODULE
    -- A submodule is a box with coordinates and length x/y.
    -- At the box edges are ports.
    type type_gui_submodule is record -- CS: read from kicad $sheet
        name                : type_submodule_name.bounded_string;
        text_size_of_name   : type_text_size;
        text_size_of_file   : type_text_size;        
		coordinates		    : type_coordinates;
        size_x, size_y      : type_grid; -- size x/y of the box
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
		coordinates_start : type_coordinates_basic;
        coordinates_end   : type_coordinates_basic;
    end record;
	package type_list_of_frame_lines is new vectors (
		index_type => positive, -- every line of a frame an id
        element_type => type_frame_line);
    
	type type_frame_text is record
		coordinates		: type_coordinates_basic;
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
        size_x, size_y  : type_grid; -- the dimensions of the frame (should fit into paper_size) 
        lines           : type_list_of_frame_lines.vector;
        texts           : type_list_of_frame_texts.vector;
    end record;

    -- there are lots of drawing frames in a schematic contained in a list
    package type_list_of_frames is new vectors (
        index_type => positive, -- every drawing fram has an id
        element_type => type_frame);

    -- TITLE BLOCK
    type type_title_block_line is record
		coordinates_start : type_coordinates_basic;
		coordinates_end   : type_coordinates_basic;
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
 		coordinates		: type_coordinates_basic;
		text			: type_device_block_text_string.bounded_string;
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

	-- The devices of a module are collected in a vector list.
 	package type_device_list_of_module is new vectors (
 		index_type => positive, -- every device of a module has an id
 		element_type => type_device);

    -- A module has a name, a list of nets and a list of devices.
    -- Objects relevant for graphical interfaces are
    -- - a list of submodules
    -- - a list of drawing frames
    -- - a list of title blocks
	type type_module is record
		name 	    : type_submodule_name.bounded_string; -- example "MOTOR_DRIVER"
		nets 	    : type_net_list_of_module.vector;
        devices     : type_device_list_of_module.vector;
        submodules  : type_list_of_gui_submodules.vector;
        frames      : type_list_of_frames.vector;
        title_blocks: type_list_of_title_blocks.vector;
        notes       : type_list_of_notes.vector;
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

