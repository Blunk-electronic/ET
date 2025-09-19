------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN PACKAGES                        --
--                                                                          --
--                              S p e c                                     --
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
--   to do:

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.maps;			use ada.strings.maps;

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_text;				use et_board_text;
with et_logging;				use et_logging;
with et_mirroring;				use et_mirroring;


package et_device_placeholders.packages is

	use pac_geometry_2;
	use pac_text_board;

	
	type type_placeholder is new type_text_fab with record
		meaning : type_placeholder_meaning := NAME;
	end record;

	-- There can be lots of placeholders of this kind. So they are stored in a list:	
	package pac_placeholders is new doubly_linked_lists (type_placeholder);
	use pac_placeholders;
	

	-- Mirrors a list of placeholders along the given axis:
	procedure mirror_placeholders (
		placeholders	: in out pac_placeholders.list;
		axis			: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of placeholders by the given angle:
	procedure rotate_placeholders (
		placeholders	: in out pac_placeholders.list;
		angle			: in type_rotation_model);

	-- Moves a list of placeholders by the given offset:
	procedure move_placeholders (
		placeholders	: in out pac_placeholders.list;
		offset			: in type_vector_model);

	
	
	-- Logs the properties of the given placeholder:
	procedure placeholder_properties (
		face			: in type_face;
		cursor			: in pac_placeholders.cursor;
		log_threshold 	: in type_log_level);
	
	
	

	-- Placeholders for device name and value can be placed in
	-- silk screen or assembly documentation only:
	type type_placeholder_layer is (SILK_SCREEN, ASSEMBLY_DOCUMENTATION);
	function to_string (layer : in type_placeholder_layer) return string;
	function to_layer (layer : in string) return type_placeholder_layer;


	
	-- Initially, when a device is added to the schematic, these placeholders are 
	-- copies of the placeholders as defined in the package model.
	-- The user is then free to change them in the 
	-- layout (position, text size, rotation, line width ...).
	type type_placeholders_silkscreen is record
		top		: pac_placeholders.list;
		bottom	: pac_placeholders.list;
	end record;

	type type_placeholders_assy_doc is record
		top		: pac_placeholders.list;
		bottom	: pac_placeholders.list;
	end record;


	-- The final package of a device in the board uses
	-- this for the actual placeholders in silkscreen and assy doc:
	type type_text_placeholders is record
		silkscreen	: type_placeholders_silkscreen;
		assy_doc	: type_placeholders_assy_doc;
	end record;

	
	
end et_device_placeholders.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
