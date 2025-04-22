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
