------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET KICAD_TO_NATIVE                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	
--		3. Make sure ports of netchangers are named like 1 or 2.

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_configuration;
with et_geometry;

with et_general;
with et_string_processing;		use et_string_processing;
with et_project;
with et_pcb;
with et_pcb_coordinates;
with et_kicad;
with et_kicad_pcb;
with et_export;
with et_csv;

package body et_kicad_to_native is


	procedure to_native (log_threshold : in et_string_processing.type_log_level) is
	-- Converts the rig (inc. component libraries) to a native project.
	-- Converts the packages (from package_libraries) to native packages.
	-- NOTE: Packages of the board (incl. their deviations from the package_libraries) are ignored !

		use et_kicad.type_rig;
		module_cursor_kicad : et_kicad.type_rig.cursor := et_kicad.type_rig.first (et_kicad.rig);

		use et_schematic.type_rig;
		module_cursor_native : et_schematic.type_rig.cursor;
		module_inserted : boolean;

		procedure copy_general_stuff (
			module_name : in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_schematic.type_module) is
		begin
			module.generic_name	:= element (module_cursor_kicad).generic_name;
			module.instance		:= element (module_cursor_kicad).instance;
			module.notes		:= element (module_cursor_kicad).notes;
			module.board		:= element (module_cursor_kicad).board;
			module.net_classes	:= element (module_cursor_kicad).net_classes;
		end copy_general_stuff;

		procedure copy_schematic_components (
			module_name : in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_schematic.type_module) is

			use et_kicad.type_components_schematic;
			components_kicad		: et_kicad.type_components_schematic.map;
			component_cursor_kicad	: et_kicad.type_components_schematic.cursor;

			use et_schematic.type_components;
			component_cursor_native	: et_schematic.type_components.cursor;
			component_inserted		: boolean;

			procedure copy_units (
			-- Copies the kicad units to the native component.
				reference	: in et_libraries.type_component_reference;
				component	: in out et_schematic.type_component) is

				use et_kicad.type_units_schematic;
				units_kicad			: et_kicad.type_units_schematic.map := element (component_cursor_kicad).units;
				unit_cursor_kicad	: et_kicad.type_units_schematic.cursor := units_kicad.first; -- point to first unit

				use et_schematic.type_units;
				unit_cursor_native	: et_schematic.type_units.cursor;
				unit_inserted		: boolean;

				unit_native_virtual	: et_schematic.type_unit (et_libraries.SCH);
				unit_native_real	: et_schematic.type_unit (et_libraries.SCH_PCB);
			begin -- copy_units
				log_indentation_up;
				
				while unit_cursor_kicad /= et_kicad.type_units_schematic.no_element loop
					log ("unit " & et_libraries.to_string (key (unit_cursor_kicad)), log_threshold + 3);

					-- depending on the appearance of the kicad component, we create a virtual or real 
					-- unit in the native schematic module:

					-- The units can be obtained by converting the kicad unit to the base unit (see et_schematic.type_units)
					-- because Kicad units are derived from this base type.
					-- Kicad stuff like path_to_package or alternative representation is discarded.
					case element (component_cursor_kicad).appearance is
						when et_libraries.SCH =>

							unit_native_virtual := et_schematic.type_unit (element (unit_cursor_kicad));
							
							et_schematic.type_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_virtual);

						when et_libraries.SCH_PCB =>

							unit_native_real := et_schematic.type_unit (element (unit_cursor_kicad));
							
							et_schematic.type_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_real);
								
					end case;
					
					next (unit_cursor_kicad);
				end loop;
				
				log_indentation_down;
			end copy_units;

		begin -- copy_schematic_components
			log_indentation_up;
			
			-- load a copy of kicad schematic components
			components_kicad := element (module_cursor_kicad).components;
			
-- 			-- copy if there are components in the kicad schematic. otherwise there is nothing to do.
-- 			if not is_empty (components_kicad) then

			-- loop in the component list of the kicad schematic module
			component_cursor_kicad := components_kicad.first;
			while component_cursor_kicad /= et_kicad.type_components_schematic.no_element loop

				log ("component " & et_libraries.to_string (key (component_cursor_kicad)), log_threshold + 2);
				
				-- depending on the appearance of the kicad component, we create a virtual or real 
				-- component in the native schematic module.
				-- Kicad stuff like power_flag is discarded.
				case element (component_cursor_kicad).appearance is
					when et_libraries.SCH =>
						
						et_schematic.type_components.insert (
							container	=> module.components,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH,
								library_name		=> element (component_cursor_kicad).library_name,
								generic_name		=> element (component_cursor_kicad).generic_name,
								value				=> element (component_cursor_kicad).value,
								commissioned		=> element (component_cursor_kicad).commissioned,
								updated				=> element (component_cursor_kicad).updated,
								author				=> element (component_cursor_kicad).author,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true

					when et_libraries.SCH_PCB => null;
						et_schematic.type_components.insert (
							container	=> module.components,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH_PCB,
								library_name		=> element (component_cursor_kicad).library_name,
								generic_name		=> element (component_cursor_kicad).generic_name,
								value				=> element (component_cursor_kicad).value,
								commissioned		=> element (component_cursor_kicad).commissioned,
								updated				=> element (component_cursor_kicad).updated,
								author				=> element (component_cursor_kicad).author,

								partcode			=> element (component_cursor_kicad).partcode,
								purpose				=> element (component_cursor_kicad).purpose,
								bom					=> element (component_cursor_kicad).bom,
								variant				=> element (component_cursor_kicad).variant,

								position			=> element (component_cursor_kicad).position,
								text_placeholders	=> element (component_cursor_kicad).text_placeholders,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true
				end case;

				-- copy the units from the kicad component to the native component
				et_schematic.type_components.update_element (
					container	=> module.components,
					position	=> component_cursor_native,
					process		=> copy_units'access);

				next (component_cursor_kicad);
			end loop;
-- 			end if;

			log_indentation_down;
		end copy_schematic_components;

		
	begin
-- 		log ("component libraries ...", log_threshold);
-- 		log_indentation_up;
		
		while module_cursor_kicad /= et_kicad.type_rig.no_element loop
			log ("module " & et_coordinates.to_string (key (module_cursor_kicad)), log_threshold + 1);
			log_indentation_up;

			-- create module
			et_schematic.type_rig.insert (
				container	=> et_schematic.rig,
				key			=> key (module_cursor_kicad),
				position	=> module_cursor_native,
				inserted	=> module_inserted -- should always be true
				);

			-- copy general stuff (notes, routing info, silk screen, documentation, net classes, ...)
			update_element (
				container	=> et_schematic.rig,
				position	=> module_cursor_native,
				process		=> copy_general_stuff'access);

			-- copy schematic components (incl. their units)
			update_element (
				container	=> et_schematic.rig,
				position	=> module_cursor_native,
				process		=> copy_schematic_components'access);
			
			
			log_indentation_down;

			next (module_cursor_kicad);
		end loop;
-- 		log_indentation_down;


-- 		log ("packages ...", log_threshold);


		
-- 		log ("schematics ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 
-- 		log_indentation_down;


-- 		log ("layouts ...", log_threshold);
-- 		log_indentation_up;
-- 
-- 
-- 		log_indentation_down;
		

		
	end to_native;
	

	
end et_kicad_to_native;

-- Soli Deo Gloria
