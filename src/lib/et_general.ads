------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GENERAL DECLARATIONS                        --
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

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.vectors;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with ada.text_io;				use ada.text_io;
--with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.calendar;				use ada.calendar;
with ada.calendar.time_zones;	use ada.calendar.time_zones;
with ada.calendar.formatting;	use ada.calendar.formatting;

package et_general is

	system_name						: constant string (1..9) := "SYSTEM ET";

	-- FILES, EXTENSIONS AND DIRECTORY NAMES

	-- The name of a project may have 100 characters which seems sufficient for now.
 	project_name_length		: constant natural := 100;

	-- All reports go into this directory:
	report_directory		: constant string (1..10)	:= "et_reports";
	report_extension		: constant string (1..3)	:= "log";
	
    -- FILE HANDLES

	
	-- FREQUENTLY USED WORDS AND PHRASES
	message_error					: constant string (1..8) := "ERROR ! ";
	message_warning					: constant string (1..10) := "WARNING ! ";	
	row_separator_single			: constant string (1..100) := 100 * "-";	
	row_separator_double			: constant string (1..100) := 100 * "=";
	
	-- COMMAND LINE SWITCHES
	--switch_about			: constant string (1..7) := "--about"; -- CS
	switch_version			: constant string (1..8)	:= "-version"; -- long switch
	switch_help				: constant string (1..5)	:= "-help"; -- long switch	
--	switch_import_file		: constant string (1..12)	:= "-import_file";	-- long switch -- currently we do not care about importing single files
	switch_import_project	: constant string (1..15)	:= "-import_project";	-- long switch
	switch_import_format	: constant string (1..14)	:= "-import_format";	-- long switch
																															
-- 	-- ACTIONS
-- 	type type_action is ( none, request_version, import_cad );

    
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length	: constant natural := 100;
	package type_person_name is new generic_bounded_length(person_name_length); use type_person_name;


-- 	-- LIBRARY NAMES AND DIRECTORIES
-- 
-- 	-- For storing bare library names like "bel_primitives" we use this bounded string:
-- 	library_name_length_max : constant natural := 100; -- CS: increase if necessary
--     package type_library_name is new generic_bounded_length(library_name_length_max); use type_library_name;
-- 
-- 	-- Bare library names can be stored further-on in an ordered set like this:
-- 	-- We use a doubly linked list because the order of the library names sometimes matters.
--     package type_list_of_library_names is new doubly_linked_lists (
-- 		element_type => type_library_name.bounded_string);
-- 
-- 	-- The base directory where libraries live is stored in a bounded string:
-- 	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
-- 	package type_library_directory is new generic_bounded_length(library_directory_length_max); use type_library_directory;
-- 
-- 	-- If a library is fully specified with path, name and extension we store them in bounded strings:
-- 	library_full_name_max : constant positive := library_directory_length_max + library_name_length_max + 4;
-- 	package type_library_full_name is new generic_bounded_length(library_full_name_max); use type_library_full_name;
-- 
-- 	-- Full library names can be stored furhter-on in an ordered set like this:
-- 	-- We use a doubly linked list because the order of the library names sometimes matters.
-- 	package type_list_of_full_library_names is new doubly_linked_lists (
-- 		element_type => type_library_full_name.bounded_string);


-- GRID AND COORDINATES
	type type_grid_extended is digits 11 range -100000000.00 .. 100000000.00;
	subtype type_grid is type_grid_extended range -100000.00 .. 100000.00; -- CS: unit assumed is MIL !!!
	-- CS: negative schematic coordinates should be forbidden
	-- type type_grid is digits 7 range 0.00 .. 100000.00; -- CS: unit assumed is MIL !!!	

    -- In general every object has at least x,y coordinates.
	type type_coordinates is tagged record
		x,y				: type_grid;
	end record;

	coordinate_zero : constant type_grid := 0.0;
	
-- PAPER SIZES
    type type_paper_size is ( A0, A1, A2, A4 ); -- CS: others
    paper_size_default : type_paper_size := A4;




-- ORIENTATION	
	-- Objects may be placed at a certain angle:
	type type_orientation is ( deg_0, deg_90, deg_180, deg_270); 
	-- other angles are not reasonable (footprints and layout have an own type for orientation)

-- NETS
	-- The name of a net may have 100 characters which seems sufficient for now.
 	net_name_length	: constant natural := 100;
	package type_net_name is new generic_bounded_length(net_name_length); use type_net_name;

	
-- COMPONENTS
	component_prefix_length_max : constant natural := 10; -- CS: there is no reason to work with longer prefixes.
	package type_component_prefix is new generic_bounded_length(component_prefix_length_max); use type_component_prefix;
	type type_component_appearance is ( 
		sch,		-- a component that exists in the schematic only (like power symbols)
		pcb,		-- a compoennt that exists on the pcb only (like a fiducial)
		sch_pcb		-- a component that exists in both schematic and soldered on a pcb
		
		-- CS: cable 
		-- CS: wire
		-- ...
		);

	type type_component is tagged record
		appearance 			: type_component_appearance;
		prefix				: type_component_prefix.bounded_string; -- together with an ID we get something like "IC702"
	end record;

	-- The name of a component is something like "IC403". Other CAE tools refer to it as "reference".
	-- It is composed of a prefix like "IC" and the number like "403":
	device_name_length_max	: constant natural := component_prefix_length_max + 100 ;
	package type_device_name is new generic_bounded_length(device_name_length_max); use type_device_name;
	
	
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

	-- Text fields:
	-- A text field may have 200 characters which seems sufficient for now.
 	text_field_length_max : constant natural := 200;
	package type_text_field_string is new generic_bounded_length(text_field_length_max); use type_text_field_string;
	type type_text_field_meaning is ( REFERENCE, VALUE, FOOTPRINT, MISC); -- CS: note, partcode, function, ...
	type type_text_field is tagged record
		meaning			        : type_text_field_meaning;
        text                    : type_text_field_string.bounded_string;
        text_attributes         : type_text_attributes;
        orientation             : type_text_orientation;
        visible                 : boolean;
        alignment_horizontal    : type_text_alignment_horizontal;
        alignment_vertical      : type_text_alignment_vertical;        
	end record;


	
-- GENERICS
	
	generic
		max : positive;
		type item is private;
	package stack_lifo is
		procedure push (x : item);
		function pop return item;
		function depth return natural;
		procedure init;
	end stack_lifo;
	
	
end et_general;

