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



	
	type type_net_label_base is tagged record
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



-- LABEL:
	
	type type_net_label is new type_net_label_base with record
		-- The position of the label is absolute (relative to drawing origin):
		position	: type_vector_model;

		-- The simple label can be read from the front or from the right.
		-- Its rotation can be HORIZONTAL or VERTICAL (0 or 90 degrees):
		rotation	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
	end record;




	function get_position (
		label : in type_net_label)
		return type_vector_model;


	function get_position (
		label : in type_net_label)
		return string;

	


	
	procedure set_moving (
		label : in out type_net_label);

	
	procedure clear_moving (
		label : in out type_net_label);


	function is_moving (
		label : in type_net_label)
		return boolean;




	
	
	package pac_net_labels is new doubly_linked_lists (type_net_label);
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
		


	

	use et_fonts;
	
	-- GUI relevant only: The font of a net label:
	net_label_font : constant type_font := (
		family	=> to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	
	-- GUI relevant only: The alignment for net labels:
	net_label_alignment : constant type_text_alignment := 
		(ALIGN_LEFT, ALIGN_BOTTOM);


	
end et_net_labels;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
