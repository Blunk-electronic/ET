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
with ada.containers.vectors;

with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_coordinates;		use et_board_coordinates;
with et_board_text;				use et_board_text;
with et_logging;				use et_logging;
with et_mirroring;				use et_mirroring;
with et_object_status;			use et_object_status;


package et_device_placeholders.packages is

	use pac_geometry_2;
	use pac_text_board;

	
	type type_text_placeholder is new type_text_fab with record
		meaning : type_placeholder_meaning := NAME;
	end record;


	function get_meaning (
		placeholder : in type_text_placeholder)
		return type_placeholder_meaning;

	
	function to_string (
		placeholder : in type_text_placeholder)
		return string;
		
	
	subtype type_placeholder_index is positive range 1 .. 10;
	
	
	function to_placeholder_index (
		index : in string)
		return type_placeholder_index;
		
		
	-- Due to packages that require a lot of area, it is useful to
	-- have a lot of placeholders for value, name, purpose.
	-- So for example the name X2 of a long edge connector can be
	-- placed on three different positions. For this reason we store
	-- the placeholders in vectors so that each of them has an index.
	-- NOTE: The index is independend of the meaning !
	package pac_text_placeholders is new vectors (
		index_type		=> type_placeholder_index,
		element_type	=> type_text_placeholder);
	
	use pac_text_placeholders;
	
	
	
	
	-- Iterates the placeholders. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		placeholders	: in pac_text_placeholders.vector;
		process			: not null access procedure (position : in pac_text_placeholders.cursor);
		proceed			: not null access boolean);

	
	
		
		
	function get_meaning (
		placeholder : in pac_text_placeholders.cursor)
		return type_placeholder_meaning;
		
		
		
	
	-- Locates the placeholder as specified by meaning and index.
	-- With other words: Locates the n'th (index) placeholder 
	-- having the given meaning.
	-- If no matching placeholder has been found, then the result
	-- is no_element:
	function locate_placeholder (
		placeholders	: in pac_text_placeholders.vector;
		meaning			: in type_placeholder_meaning;
		index			: in type_placeholder_index)
		return pac_text_placeholders.cursor;
		
		

	-- Mirrors a list of placeholders along the given axis:
	procedure mirror_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		axis			: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of placeholders by the given angle:
	procedure rotate_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		angle			: in type_rotation_model);

	-- Moves a list of placeholders by the given offset:
	procedure move_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		offset			: in type_vector_model);

	
	
	-- Logs the properties of the given placeholder:
	procedure placeholder_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in type_log_level);
	
	
	

	-- Placeholders for device name and value can be placed in
	-- silk screen or assembly documentation only:
	type type_placeholder_layer is (SILKSCREEN, ASSY_DOC);
	-- CS apply prefix !
	
	function to_string (
		layer : in type_placeholder_layer) 
		return string;

		
	function to_placeholder_layer (
		layer : in string) 
		return type_placeholder_layer;


		
		

	
	-- Initially, when a device is added to the schematic, these placeholders are 
	-- copies of the placeholders as defined in the package model.
	-- The user is then free to change them in the 
	-- layout (position, text size, rotation, line width ...).
	type type_placeholders_silkscreen is record
		top		: pac_text_placeholders.vector;
		bottom	: pac_text_placeholders.vector;
	end record;

	type type_placeholders_assy_doc is record
		top		: pac_text_placeholders.vector;
		bottom	: pac_text_placeholders.vector;
	end record;


	-- The final package of a device in the board uses
	-- this for the actual placeholders in silkscreen and assy doc:
	type type_text_placeholders is record
		silkscreen	: type_placeholders_silkscreen;
		assy_doc	: type_placeholders_assy_doc;
	end record;



	-- Returns the placeholders as a string:
	function to_string (
		placeholders : in type_text_placeholders)
		-- CS parameter to insert linebreaks
		return string;

	
	
	-- Moves the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the placeholder
	-- is moved to the given point.
	-- If coordinates is relative, then the placeholder
	-- is moved by the x/y-distance given by point:
	procedure move_placeholder (
		placeholders	: in out type_text_placeholders;
		meaning			: in type_placeholder_meaning;					 
		layer			: in type_placeholder_layer; -- silkscreen, assy doc
		face			: in type_face;
		index			: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model); -- x/y


	
	-- Rotates the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the placeholder
	-- is rotated to the given rotation.
	-- If coordinates is relative, then the placeholder
	-- is rotated by the given rotation:
	procedure rotate_placeholder (
		placeholders	: in out type_text_placeholders;
		meaning			: in type_placeholder_meaning;					 
		layer			: in type_placeholder_layer; -- silkscreen, assy doc
		face			: in type_face;
		index			: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in type_rotation_model);



	-- Sets the proposed-flag of all placeholders which are in the
	-- given zone around the given place.
	-- Since the placeholders have a position that is relative to
	-- the parent package, the absolute package position must also
	-- be provided.
	-- Adds to count the number of placeholders that have been found:
	procedure propose_placeholders (
		placeholders		: in out type_text_placeholders;
		package_position	: in type_package_position;
		catch_zone			: in type_catch_zone;
		count				: in out natural;
		log_threshold		: in type_log_level);
		
		
		
	-- Modifies the status of a placeholder among a given
	-- list of placeholders. The placeholder is identified by
	-- the layer, face and the cursor to the placeholder:
	procedure modify_status (
		placeholders		: in out type_text_placeholders;
		layer				: in type_placeholder_layer;
		face				: in type_face;
		placeholder_cursor	: in pac_text_placeholders.cursor;
		operation			: in type_status_operation);

		
	-- Resets all status flags of the given placeholders:
	procedure reset_status (
		placeholders		: in out type_text_placeholders);
		
		
	-- Returns the first placeholder among the given placeholders
	-- that has the given status flag set.
	-- If nothing has been found, then placeholder_cursor is no_element.
	-- Searches in this order: silkscreen top/bottom, assy_doc top/bottom:
	procedure get_first_placeholder (
		placeholders		: in type_text_placeholders;
		flag				: in type_flag;
		placeholder_cursor	: out pac_text_placeholders.cursor;
		layer				: out type_placeholder_layer; -- silkscreen, assy_doc
		face				: out type_face; -- top/bottom
		index				: out type_placeholder_index; -- 1, 2, 3, ...
		log_threshold		: in type_log_level);
		
		
	-- This stuff is required when cursors of placeholders
	-- are to be collected:
		
	type type_placeholder_cursor is record
		cursor : pac_text_placeholders.cursor;
		index  : type_placeholder_index;
	end record;
	
		
	package pac_placeholder_cursors is 
		new doubly_linked_lists (type_placeholder_cursor);
		
		
	type type_placeholder_cursors_assy is record
		top		: pac_placeholder_cursors.list;
		bottom	: pac_placeholder_cursors.list;
	end record;

	
	type type_placeholder_cursors_silkscreen is record
		top		: pac_placeholder_cursors.list;
		bottom	: pac_placeholder_cursors.list;
	end record;

		
		
	type type_placeholder_cursors is record
		assy_doc	: type_placeholder_cursors_assy;
		silkscreen	: type_placeholder_cursors_silkscreen;
	end record;
		
		
	-- Extracts from the given placeholders those
	-- which have the given status flag set.
	-- The result are cursors of placeholders:
	function get_placeholder_cursors (
		placeholders	: in type_text_placeholders;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_placeholder_cursors;
		
	
end et_device_placeholders.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
