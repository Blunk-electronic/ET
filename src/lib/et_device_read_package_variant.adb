------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE READ / PACKAGE VARIANT                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
-- To Do:
-- - clean up
--
--

with ada.text_io;					use ada.text_io;
with ada.strings; 					use ada.strings;

with ada.directories;
with ada.exceptions;

with et_file_sections;				use et_file_sections;
with et_package_read;

with et_package_name;				use et_package_name;

with et_package_library;
with et_package_variant_name;		use et_package_variant_name;

with et_package_model_name;			use et_package_model_name;

with et_unit_name;
with et_port_names;
with et_terminal_name;
with et_package_variant_terminal_port_map;

with et_keywords;					use et_keywords;



package body et_device_read_package_variant is
	
	
-- PACKAGE VARIANT:
	
	use pac_package_variant_name;
	variant_name : pac_package_variant_name.bounded_string; -- N, D



		
		
	procedure read_package_variant (
		line 			: in type_fields_of_line;
   		check_layers	: in type_layer_check := (check => NO);
		log_threshold	: in type_log_level)
	is
		use ada.directories;
		use et_package_read;		
		use et_package_library;
		
		kw : string := f (line, 1);
		
		package_model_name : pac_package_model_file.bounded_string;		
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name D
			expect_field_count (line, 2);
			check_variant_name_length (f (line, 2));
			variant_name := to_variant_name (f (line, 2));
			log (text => "variant " & to_string (variant_name), level => log_threshold);
			
			
		elsif kw = keyword_package_model then -- package_model libraries/packages/S_SO14.pac
			expect_field_count (line, 2);

			-- The given path is something like libraries/packages/S_SO14.pac.
			-- Check if the package name like S_SO14 is too long or contains invalid characters.
			check_package_name_length (base_name (f (line, 2)));
			check_package_name_characters (to_package_name (base_name (f (line, 2))));

			package_model_name := to_package_model_name (f (line, 2));
			log (text => "package model " & to_string (package_model_name), level => log_threshold);
							
			-- Read package model 
			-- (like libraries/packages/__#__#lbr#bel_ic_pretty#S_SO14.pac)
			-- and do a conductor layer check if required.
			read_package (
				file_name		=> package_model_name, 
				check_layers	=> check_layers, 
				log_threshold	=> log_threshold);

			-- Create the link to the package model in the
			-- package library:
			variant.model_cursor := get_package_model (package_model_name);
		else
			invalid_keyword (kw);
		end if;
	end read_package_variant;




	
		
		
	procedure insert_package_variant is
		use pac_package_variants;
		
		inserted : boolean;
		position : pac_package_variants.cursor;
	begin
		check_variant_name_characters (variant_name); 
		-- CS move to procedure read_package_variant

		insert (
			container	=> variants,
			key			=> variant_name, -- N, D 
			inserted	=> inserted,
			position	=> position,
			new_item	=> variant);

		-- A particular variant must occur only once in the device model:
		if not inserted then
			log (ERROR, "variant " & to_string (variant_name) 
				& " already used !");
				
			raise constraint_error;
		end if;

		-- clean up for next variant
		variant := (others => <>);
	end insert_package_variant;


	
	
	
	
	
	
-- TERMINAL-PORT-MAP:
	
	use et_package_variant_terminal_port_map;
	terminal_port_map : pac_terminal_port_map.map;	
	
	
	procedure read_terminal_port_assignment (
		line 			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		use et_unit_name;
		use et_port_names;
		use et_terminal_name;		
		
		use pac_terminal_port_map;
		
		inserted	: boolean;
		position	: pac_terminal_port_map.cursor;

		terminal	: pac_terminal_name.bounded_string; -- H5, 14
		unit		: pac_unit_name.bounded_string; -- PWR, IO_BANK_2
		port		: pac_port_name.bounded_string; -- VCC

		place : type_field_count_positive := 1; -- the field being read from given line

		-- CS: detect missing parameters
		-- CS: warn about wrong misplaced keywords
		-- CS: test if terminal, unit and port exist
	begin
		expect_field_count (line, 6); -- terminal 14 unit 5 port VCC
		
		while place <= get_field_count (line) loop
		
			-- We expect the terminal name after the keyword "terminal"
			if f (line, place) = keyword_terminal then
				terminal := to_terminal_name (f (line, place + 1)); -- 14

			-- After the keyword "unit" must come the unit name:
			elsif f (line, place) = keyword_unit then 
				unit := to_unit_name (f (line, place + 1)); -- 5

			-- After the keyword "port" must come the port name
			elsif f (line, place) = keyword_port then 
				port := to_port_name (f (line, place + 1)); -- VCC
				
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;

		
		
		-- insert terminal to port assigment in temporarily terminal_port_map
		insert (
			container	=> terminal_port_map,
			key			=> terminal, -- H5, 14
			inserted	=> inserted,
			position	=> position,
			new_item	=> (
							unit	=> unit, -- IO_BANK_2,
							name	=> port -- VCC
							));

		-- an assigment must be unique !
		if not inserted then
			log (ERROR, "terminal-to-port assigment already used !");
			
			raise constraint_error;
		end if;

		-- clean up for next terminal to port assigment
		terminal	:= to_terminal_name ("");
		unit		:= to_unit_name ("");
		port		:= to_port_name ("");
		
	end read_terminal_port_assignment;

		
		
	
	
	
	
	procedure assign_terminal_port_map is begin
		-- copy temporarily terminal_port_map to current variant
		variant.terminal_port_map := terminal_port_map;

		-- clean up temporarily terminal_port_map for next variant
		pac_terminal_port_map.clear (terminal_port_map);

	end assign_terminal_port_map;
	
	
	
end et_device_read_package_variant;
