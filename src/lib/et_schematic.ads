------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 
--		1. Objects like net segments, net labels, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required
--		3. device accessories

with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;

with et_nets;					use et_nets;
with et_net_names;				use et_net_names;
with et_sheets;					use et_sheets;
with et_coordinates_2;			use et_coordinates_2;
with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;

with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;
with et_device_placeholders.packages;

with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;
with et_submodules;
with et_material;
with et_netlists;
with et_text;
with et_symbols;						use et_symbols;
with et_port_names;						use et_port_names;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model_names;				use et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_device_partcode;				use et_device_partcode;
with et_device_library;					use et_device_library;
with et_package_names;					use et_package_names;
with et_package_variant;				use et_package_variant;
with et_terminals;						use et_terminals;
with et_packages;						use et_packages;
with et_commit;
with et_object_status;					use et_object_status;
with et_unit_name;						use et_unit_name;
with et_units;							use et_units;
with et_fonts;							use et_fonts;


package et_schematic is

	use pac_net_name;
	use pac_unit_name;
	
	use pac_geometry_2;



	-- GUI relevant only: The font of a text/note in the schematic:
	text_font : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);

	
	-- A text/note in the schematic:
	type type_text is new pac_text_schematic.type_text with record
		position	: type_vector_model;
		rotation	: et_text.type_rotation_documentation := et_text.HORIZONTAL;
		sheet		: type_sheet := type_sheet'first;
		content		: et_text.pac_text_content.bounded_string;
		--font		: et_text.type_font;
	end record;
		
	package pac_texts is new doubly_linked_lists (type_text);


	

	


	-- This is a device as it appears in the schematic.
	type type_device_sch (appearance : type_appearance_schematic) is record

		-- The link to the device model like ../libraries/devices/transistor/pnp.dev
		model	: pac_device_model_file.bounded_string;
		-- CS use a cursor to the model instead ?

		-- The units like PWR, A, B, ...
		-- Virtual devices have only one unit (like the GND symbol).
		-- Real devices like a single resistor have one unit.
		-- Real devices like FPGAs have many units (like PWR1, PWR2, GPIO1, GPIO2, ...):
		units	: pac_units.map;
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when APPEARANCE_PCB =>
				value		: pac_device_value.bounded_string; -- 470R
				
				partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				-- For virtual packages (test points, edge connectors, ...)
				-- usually no partcode is required.

				-- The purpose indicates what the device is doing.
				-- It is usually required for devices that require interaction
				-- with the user of a PCBA:
				purpose		: pac_device_purpose.bounded_string; -- brightness_control

				-- The package variant:
				variant		: pac_package_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout, the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a package is by default TOP.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means it will be drawn
				-- mirrored along its Y-axis.
				position			: et_pcb_coordinates_2.type_package_position; -- incl. rotation and face
				text_placeholders	: et_device_placeholders.packages.type_text_placeholders;

				-- CS flags that signal whether partcode, purpose, bom are displayed or not.

				-- The status of the package:
				status : type_object_status;
				
			when APPEARANCE_VIRTUAL => null;

		end case;
	end record;




	-- The devices of a module are collected in a map.
	-- CS: This must be a hashed map:
 	package pac_devices_sch is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device_sch);

	use pac_devices_sch;
	

-- DEVICE STATUS OPERATIONS:
	
	-- NOTE: Operations regarding the status
	-- apply to the package of the device (in the board domain).
	-- Status opertions for individual units (in the schematic)
	-- are specified in the package et_units.
	-- Regarding set and clear operations: If the device
	-- is not real (APPEARANCE_PCB) then the operation has no effect.
	-- Regarding query operations like is_selected or is_moving: If
	-- the device is not real (APPEARANCE_PCB) then the return is
	-- always false.
	
	procedure set_selected (
		device : in out type_device_sch);
	

	procedure clear_selected (
		device : in out type_device_sch);
	

	function is_selected (
		device : in type_device_sch)
		return boolean;
	

	
	procedure set_proposed (
		device : in out type_device_sch);
	

	procedure clear_proposed (
		device : in out type_device_sch);

	
	function is_proposed (
		device : in type_device_sch)
		return boolean;



	
	procedure set_moving (
		device : in out type_device_sch);
	

	procedure clear_moving (
		device : in out type_device_sch);

	
	function is_moving (
		device : in type_device_sch)
		return boolean;


	
	
	procedure modify_status (
		device		: in out type_device_sch;
		operation	: in type_status_operation);
	

	
	procedure reset_status (
		device : in out type_device_sch);




	-- Returns true if given device is real (means if it has a physical 
	-- counterpart in the PCB layout). For a resistor it returns true.
	-- For a GND symbol it returns false:
	function is_real (
		device : in pac_devices_sch.cursor) 
		return boolean;
	

	function is_proposed (
		device : in pac_devices_sch.cursor)
		return boolean;
	

	function is_selected (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	function is_moving (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	
	-- Iterates the devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean);

	

	
-- DEVICE QUERY OPERATIONS:
	
	
	-- Returns the name of the device indicated by
	-- the given cursor:
	function to_string (
		device_cursor : in pac_devices_sch.cursor)
		return string;
		


	
	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether certain
	-- devices should be excluded (because they may not be present in a particular
	-- assembly variant).
	-- NOTE: If no variant is given, then the default variant is assumend
	-- and ALL devices are returned.
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports;


	

	-- Maps from schematic device to device model (in library):
	function get_device_model (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor;


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model ( -- CS rename to get_package_model_name
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
	

	-- Returns the cursor to the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_models.cursor;


	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. 
	-- Otherwise a constraint error is raised.
	function has_real_package (
		device : in pac_devices_sch.cursor)
		return boolean;

	
	-- Returns the value of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_value (
		device : in pac_devices_sch.cursor)
		return pac_device_value.bounded_string;

	
	-- Returns the purpose of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_purpose (
		device : in pac_devices_sch.cursor)
		return pac_device_purpose.bounded_string;


	-- Returns the partcode of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_partcode (
		device : in pac_devices_sch.cursor)
		return pac_device_partcode.bounded_string;



	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_package_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string;



	-- Maps from the given terminal to the linked port and unit.
	-- The given device must be real. Otherwise a constraint error
	-- will be raised:
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string) -- H7, 1, 14
		return type_get_port_result;


	-- Maps from the given device cursor, unit and port name 
	-- to a cursor of the linked terminal.
	-- A port is always linked with a terminal.
	-- The given device must be real. Otherwise a constraint error will be raised:
	function get_terminal (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return et_terminals.pac_terminals.cursor;

	

	
	
	-- COMMITS OF ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_device_commit is new pac_commit (pac_devices_sch.map);
	use pac_device_commit;
	
	package pac_device_commits is new doubly_linked_lists (
		element_type	=> pac_device_commit.type_commit);

	type type_devices_undo_redo_stack is record
		dos		: pac_device_commits.list;
		redos	: pac_device_commits.list;
	end record;

	
	
		
	

	
	
	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;

	-- As there are assembly variants, for each of them a dedicated netlist must be generated.
	package pac_netlists is new ordered_maps (
		key_type		=> pac_assembly_variant_name.bounded_string, -- low_cost, empty if default variant
		"<"				=> pac_assembly_variant_name."<",
		element_type	=> et_netlists.pac_netlist.tree, -- provides info on primary and secondary net dependencies
		"="				=> et_netlists.pac_netlist."=");





	-- To distinguish between electrical and non-electrical devices
	-- use this type:
	type type_device_category is (ELECTRICAL, NON_ELECTRICAL);
	

	
	procedure device_name_in_use (
		name	: in type_device_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category);	-- electrical/non-electrical
	
		
end et_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
