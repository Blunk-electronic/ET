------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES NON-ELECTRICAL                           --
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
--   to do:
--


with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_packages;						use et_packages;
with et_package_names;					use et_package_names;
with et_device_name;					use et_device_name;
with et_pcb_stack;						use et_pcb_stack;
with et_commit;
with et_object_status;					use et_object_status;
with et_mirroring;						use et_mirroring;


package et_devices_non_electrical is
	
	use pac_geometry_2;



	-- In this world, if a package is flipped, then it is
	-- mirrored along the Y-axis.
	-- This function maps from flip status to mirror along y-axis.
	-- If flipped is false, then the return is MIRRROR_NO.
	-- If flipped is true, then the return is MIRROR_ALONG_Y_AXIS:
	function to_mirror_along_y_axis ( -- CS remove ?
		flipped : in type_flipped)
		return type_mirror;


	

	-- Devices which do not have a counterpart in the schematic 
	-- (like fiducials, mounting holes, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or FD (fiducial).
	-- This is NOT about accessories of the module !
	-- These devices do NOT appear in the BOM !
	-- We collect them in an indefinite ordered map.
	-- CS: Move into a separate package !
	
	type type_device_non_electric is record
		position			: et_pcb_coordinates_2.type_package_position; -- incl. rotation and face
		text_placeholders	: type_text_placeholders;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		-- CS cursor to package model instead ?

		-- CS ?
		--value		: pac_device_value.bounded_string; -- 470R
		--partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		--purpose		: pac_device_purpose.bounded_string; -- brightness_control

		status : type_object_status;
	end record;
	

	
	procedure set_proposed (
		device : in out type_device_non_electric);


	procedure clear_proposed (
		device : in out type_device_non_electric);


	function is_proposed (
		device : in type_device_non_electric)
		return boolean;


		

	
	procedure set_selected (
		device : in out type_device_non_electric);

	
	procedure clear_selected (
		device : in out type_device_non_electric);

	
	function is_selected (
		device : in type_device_non_electric)
		return boolean;


	

	procedure set_moving (
		device : in out type_device_non_electric);


	procedure clear_moving (
		device : in out type_device_non_electric);

	
	function is_moving (
		device : in type_device_non_electric)
		return boolean;


	

	
	procedure set_locked (
		device : in out type_device_non_electric);


	procedure clear_locked (
		device : in out type_device_non_electric);

	
	function is_locked (
		device : in type_device_non_electric)
		return boolean;



	procedure modify_status (
		device		: in out type_device_non_electric;
		operation	: in type_status_operation);



	procedure reset_status (
	   device : in out type_device_non_electric);

	
	
	-- CS: this should be a hashed map:
	package pac_devices_non_electric is new ordered_maps ( -- CS rename to pac_devices_non_electrical
		key_type		=> type_device_name, -- H1, FD2, ...
		element_type	=> type_device_non_electric);

	use pac_devices_non_electric;
	

	-- Returns the name of the non-electical device:
	function to_string (
		device : in pac_devices_non_electric.cursor)
		return string;
	
		
	function is_proposed (
		device : in pac_devices_non_electric.cursor)
		return boolean;
	

	function is_selected (
		device : in pac_devices_non_electric.cursor)
		return boolean;


	function is_moving (
		device : in pac_devices_non_electric.cursor)
		return boolean;
	

	function is_locked (
		device : in pac_devices_non_electric.cursor)
		return boolean;

	

	
	-- COMMITS OF NON-ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_non_electrical_device_commit is new pac_commit (pac_devices_non_electric.map);
	use pac_non_electrical_device_commit;
	
	package pac_non_electrical_device_commits is new doubly_linked_lists (
		element_type	=> pac_non_electrical_device_commit.type_commit);

	type type_non_electrical_devices_undo_redo_stack is record
		dos		: pac_non_electrical_device_commits.list;
		redos	: pac_non_electrical_device_commits.list;
	end record;
	


	
	-- Iterates the non-electric devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean);



	
end et_devices_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
