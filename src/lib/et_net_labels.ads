------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NETS LABELS                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

--with ada.text_io;				use ada.text_io;
with ada.characters.handling;	use ada.characters.handling;
--with ada.strings; 				use ada.strings;
--with ada.strings.maps;			use ada.strings.maps;
--with ada.strings.fixed; 		use ada.strings.fixed;
--with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers; 			use ada.containers;
--with ada.containers.vectors;
--with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with cairo;						--use cairo;

with et_coordinates;			use et_coordinates;
with et_text;					use et_text;
with et_symbols;				use et_symbols;
--with et_string_processing;		use et_string_processing;


package et_net_labels is

	use pac_geometry_2;
	
	
	type type_net_label_appearance is (
		SIMPLE,	-- a label that shows just the name of the net
		TAG 	-- a lable that shows the net name, the sheet name and the row/column
		);		-- where the net continues

	
	function to_string (appearance : in type_net_label_appearance) return string;
	
	function to_appearance (appearance : in string) return type_net_label_appearance;

	
	type type_net_label_direction is (INPUT, OUTPUT, BIDIR, TRISTATE, PASSIVE); -- CS POWER ?
	net_label_direction_default : constant type_net_label_direction := PASSIVE;

	
	function to_string (direction : in type_net_label_direction) return string;
	function to_direction (direction : in string) return type_net_label_direction;

	keyword_direction : constant string := "direction";

	
	type type_net_label_base is tagged record
		-- The position of the label is absolute (relative to drawing origin):
		position	: type_point;
		
        size		: et_symbols.pac_text.type_text_size := et_symbols.text_size_default;
		width		: et_symbols.type_text_line_width := et_symbols.type_text_line_width'first;
	end record;

	
	type type_net_label (appearance : type_net_label_appearance) is new type_net_label_base with record
		case appearance is
			when TAG => 
				direction		: type_net_label_direction := net_label_direction_default;

				-- A tag label can only be attached to a stub of a net, means to a dead end of a net segment.
				-- The rotation of the label should depend on the direction of the stub. 
				-- The rotation is about its own position. 
				-- However, the shown text inside the label (net name and coordinates) is always readable
				-- from the front or from the right.
				rotation_tag	: type_rotation_relative := 0.0;

			when SIMPLE =>
				-- The simple label can be read from the front or from the right.
				-- Its rotation can be HORIZONTAL or VERTICAL (0 or 90 degrees):
				rotation_simple	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
		end case;
	end record;

	package pac_net_labels is new indefinite_doubly_linked_lists (type_net_label);


	
	-- GUI relevant only: The font of a net label:
	net_label_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	
	-- GUI relevant only: The alignment for simple labels:
	net_label_alignment : constant et_text.type_text_alignment := (et_text.LEFT, et_text.BOTTOM);


	use pac_geometry_sch;
	
	-- GUI relevant only: The line width of the box that enshroudes the net name of a tag label:
	tag_label_box_line_width : constant type_distance_positive := 0.2;

	
	-- GUI relevant only: The spacing between anchor point of tag label and net name:
	tag_label_text_offset : constant type_float_internal_positive := 1.0;

	
	-- GUI relevant only: The ratio of box height to text size of a tag label:
	tag_label_height_to_size_ratio : constant type_float_internal_positive := 1.8;


	
end et_net_labels;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
