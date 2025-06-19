------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NETS LABELS                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

with ada.characters.handling;			use ada.characters.handling;
with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;
with cairo;

with et_schematic_coordinates;			use et_schematic_coordinates;
with et_text;							use et_text;
with et_schematic_text;					use et_schematic_text;

with et_alignment;						use et_alignment;
with et_object_status;					use et_object_status;

with et_fonts;



package et_net_labels is

	use pac_geometry_2;
	use pac_text_schematic;
	
	
	type type_net_label_appearance is (
		SIMPLE,	-- a label that shows just the name of the net
		TAG 	-- a lable that shows the net name, the sheet name and the row/column
		);		-- where the net continues
	-- CS rename to type_label_category

	
	function to_string (appearance : in type_net_label_appearance) return string;
	
	function to_appearance (appearance : in string) return type_net_label_appearance;

	
	type type_net_label_direction is (INPUT, OUTPUT, BIDIR, TRISTATE, PASSIVE); -- CS POWER ?
	net_label_direction_default : constant type_net_label_direction := PASSIVE;

	
	function to_string (direction : in type_net_label_direction) return string;
	function to_direction (direction : in string) return type_net_label_direction;


	
	type type_net_label_base is tagged record -- CS rename to type_net_label
		size		: type_text_size := text_size_default;
		
		width		: et_schematic_text.type_text_line_width := et_schematic_text.type_text_line_width'first;
		-- CS probably no need ?
		
		status		: type_object_status;
	end record;



	procedure set_proposed (
		label : in out type_net_label_base);

	
	procedure clear_proposed (
		label : in out type_net_label_base);


	function is_proposed (
		label : in type_net_label_base)
		return boolean;

	


	procedure set_selected (
		label : in out type_net_label_base);

	
	procedure clear_selected (
		label : in out type_net_label_base);


	function is_selected (
		label : in type_net_label_base)
		return boolean;


	

	procedure modify_status (
		label 		: in out type_net_label_base;
		operation	: in type_status_operation);						
	

	procedure reset_status (
		label : in out type_net_label_base);



-- SIMPLE LABEL:
	
	type type_net_label_simple is new type_net_label_base with record
		-- The position of the label is absolute (relative to drawing origin):
		position	: type_vector_model;

		-- The simple label can be read from the front or from the right.
		-- Its rotation can be HORIZONTAL or VERTICAL (0 or 90 degrees):
		rotation	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
	end record;




	function get_position (
		label : in type_net_label_simple)
		return type_vector_model;


	function get_position (
		label : in type_net_label_simple)
		return string;

	


	
	procedure set_moving (
		label : in out type_net_label_simple);

	
	procedure clear_moving (
		label : in out type_net_label_simple);


	function is_moving (
		label : in type_net_label_simple)
		return boolean;




	
	
	package pac_net_labels is new doubly_linked_lists (type_net_label_simple);
	use pac_net_labels;
	

	function get_position (
		label : in pac_net_labels.cursor)
		return type_vector_model;


	function get_position (
		label : in pac_net_labels.cursor)
		return string;

	
	
	function is_proposed (
		label : in pac_net_labels.cursor)
		return boolean;


	function is_selected (
		label : in pac_net_labels.cursor)
		return boolean;


	function is_moving (
		label : in pac_net_labels.cursor)
		return boolean;


	-- Appends the list of secondary labels to
	-- the list of primary labels:
	procedure merge_labels (
		primary		: in out pac_net_labels.list;
		secondary	: in out pac_net_labels.list);
		


-- TAG LABEL:	

	
	type type_net_label_tag (active : boolean := false) is record
	-- is new type_net_label_base with record
		case active is
			when TRUE =>
				size		: type_text_size := text_size_default;
				
				-- width		: et_schematic_text.type_text_line_width := et_schematic_text.type_text_line_width'first;
				-- CS probably no need ?
				
				status		: type_object_status;
				
				direction	: type_net_label_direction := net_label_direction_default;

				-- A tag label can only be attached to a stub of a net, means to a dead end of a net segment.
				-- The rotation of the label should depend on the direction of the stub. 
				-- The rotation is about its own position. 
				-- However, the shown text inside the label (net name and coordinates) is always readable
				-- from the front or from the right.
				rotation	: type_rotation_relative := 0.0;
				-- CS probably no need ?

			when FALSE => null;
		end case;
	end record;


	
	procedure modify_status (
		label 		: in out type_net_label_tag;
		operation	: in type_status_operation);


	
	type type_tag_labels is record
		A : type_net_label_tag; --(active => false);
		B : type_net_label_tag; -- (active => false);
	end record;



	

	procedure reset_status (
		labels : in out type_tag_labels);
	

	function is_active (
		label : in type_net_label_tag)
		return boolean;


	procedure set_active (
		label : in out type_net_label_tag);
	


	function is_proposed (
		label : in type_net_label_tag)
		return boolean;

	
							 
	procedure set_proposed (
		label : in out type_net_label_tag);


	procedure clear_proposed (
		label : in out type_net_label_tag);


	

	function is_selected (
		label : in type_net_label_tag)
		return boolean;

	
	procedure set_selected (
		label : in out type_net_label_tag);


	procedure clear_selected (
		label : in out type_net_label_tag);





	function is_moving (
		label : in type_net_label_tag)
		return boolean;

	
	procedure set_moving (
		label : in out type_net_label_tag);


	procedure clear_moving (
		label : in out type_net_label_tag);

	
	
	
	procedure reset_tag_label (
		label : in out type_net_label_tag);


	

	use et_fonts;
	
	-- GUI relevant only: The font of a net label:
	net_label_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	
	-- GUI relevant only: The alignment for simple labels:
	net_label_alignment : constant type_text_alignment := 
		(ALIGN_LEFT, ALIGN_BOTTOM);


	use pac_geometry_sch;
	
	-- GUI relevant only: The line width of the box that enshroudes the net name of a tag label:
	tag_label_box_line_width : constant type_distance_positive := 0.2;

	
	-- GUI relevant only: The spacing between anchor point of tag label and net name:
	tag_label_text_offset : constant type_distance_positive := 1.0;

	
	-- GUI relevant only: The ratio of box height to text size of a tag label:
	tag_label_height_to_size_ratio : constant type_distance_positive := 1.8;


	
end et_net_labels;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
