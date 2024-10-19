------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          CANVAS DRAWING FRAME                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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


with et_project;
with et_project.modules;
with et_assembly_variants;



package body et_canvas.drawing_frame is


	
	function to_distance (
		d : in et_frames.type_distance)
		return pac_geometry.type_distance
	is begin
		return pac_geometry.type_distance (d);
	end to_distance;


	
	function to_vector (
		p : in et_frames.type_position)
		return type_vector_model
	is
		r : type_vector_model;
	begin
		r.x := to_distance (p.x);
		r.y := to_distance (p.y);
		
		return r;
	end to_vector;



	function to_line (
		l : in et_frames.type_line)
		return pac_geometry.type_line
	is
		result : pac_geometry.type_line;
	begin
		result.start_point := to_vector (l.start_point);
		result.end_point   := to_vector (l.end_point);
		return result;
	end to_line;


	
	
	procedure draw_frame (
		frame : in type_frame_general'class)
	is
		-- This is a temporarily line that is used to
		-- draw all the individual lines of the frame:
		l : pac_geometry.type_line;

		-- Get the width of the frame:
		w : constant pac_geometry.type_distance_positive := 
			to_distance (frame.size.x);

		-- Get the height of the frame:
		h : constant pac_geometry.type_distance_positive := 
			to_distance (frame.size.y);

		-- Get the position of the lower-left corner
		-- of the frame:
		p : constant pac_geometry.type_position := (
			place => to_vector (frame.position),
			rotation => zero_rotation);

		-- Get the width of the border of the frame:
		b : constant pac_geometry.type_distance_positive := 
			to_distance (frame.border_width);

		
		-- Draws the temporarily line. Takes into account
		-- the position of the lower-left corner of the frame:
		procedure draw_line is begin
			-- The width of 0.0 has no meaning because 
			-- the argument do_stroke is false by default
			-- (see specs of draw_line):
			draw_line (
				line		=> l,
				pos			=> p,
				width		=> 0.0);
		end draw_line;


		procedure outer_border is begin
			set_linewidth (linewidth_2);

			-- Assemble the lower line:
			l.start_point := (0.0, 0.0);
			l.end_point := (w, 0.0);
			draw_line;

			-- Assemble the right line:
			l.start_point := (w, 0.0);
			l.end_point := (w, h);
			draw_line;

			-- Assemble the upper line:
			l.start_point := (w, h);
			l.end_point := (0.0, h);
			draw_line;

			-- Assemble the left line:
			l.start_point := (0.0, h);
			l.end_point := (0.0, 0.0);
			draw_line;

			stroke;
		end outer_border;


		procedure inner_border is begin
			set_linewidth (linewidth_2);

			-- Assemble the lower line:
			l.start_point := (b, b);
			l.end_point := (w - b, b);
			draw_line;

			-- Assemble the right line:
			l.start_point := (w - b, b);
			l.end_point := (w - b, h - b);
			draw_line;

			-- Assemble the upper line:
			l.start_point := (w - b, h - b);
			l.end_point := (b, h - b);
			draw_line;

			-- Assemble the left line:
			l.start_point := (b, h - b);
			l.end_point := (b, b);
			draw_line;

			stroke;
		end inner_border;


		
		procedure sector_delimiters is

			sector_width  : constant pac_geometry.type_distance_positive := 
				(w - 2 * b) / pac_geometry.type_distance_positive (frame.sectors.columns);
			
			sector_height : constant pac_geometry.type_distance_positive := 
				(h - 2 * b) / pac_geometry.type_distance_positive (frame.sectors.rows);
			
			use et_text;

			
			procedure draw_index (
				content	: in pac_text_content.bounded_string;
				pos		: in type_vector_model) 
			is 
				use pac_draw_text;
			begin
				draw_text (
					content		=> content,
					size		=> pac_geometry.type_distance_positive (font_indexes_size),
					font		=> font_indexes,

					-- The anchor point is offset by the position
					-- of the frame:
					anchor		=> add (pos, p.place),
					
					origin		=> false,
					rotation	=> 0.0,
					alignment	=> (CENTER, CENTER));
			end draw_index;

			
			x, y  	: pac_geometry.type_distance_positive;
			xo, yo	: pac_geometry.type_distance;
			

			
		begin -- draw_sector_delimiters
			
			set_linewidth (linewidth_1);

			
			-- COLUMN DELIMITERS:
			-- The lines are drawn upwards, from bottom to top.
			for i in 1 .. frame.sectors.columns - 1 loop

				-- compute x coordinate
				x := pac_geometry.type_distance_positive (i) * sector_width
					+ b; -- offset to the right

				-- LOWER BORDER
				
				-- draw the line bottom-up:
				-- lower end:
				l.start_point := type_vector_model (set (
					x => x,
					y => zero));

				-- upper end:
				l.end_point := type_vector_model (set (
					x => x,
					y => b));

				draw_line;


				
				-- UPPER BORDER
				-- draw the line bottom-up:
				-- lower end:
				l.start_point := type_vector_model (set (
					x => x,
					y => h - b));

				-- upper end:
				l.end_point := type_vector_model (set (
					x => x,
					y => h));
				
				draw_line;
			end loop;

			
			-- ROW DELIMITERS:
			-- The lines are drawn from the left to the right.
			for i in 1 .. frame.sectors.rows - 1 loop

				-- compute y coordinate
				y := pac_geometry.type_distance_positive (i) * sector_height
					+ b; -- offset upwards

				-- LEFT BORDER
				
				-- draw the line from the left to the right:
				-- left end:
				l.start_point := type_vector_model (set (
					x => zero,
					y => y));

				-- right end:
				l.end_point := type_vector_model (set (
					x => b,
					y => y));

				draw_line;
				
				-- RIGHT BORDER
				-- draw the line from the left to the right:
				-- left end:
				l.start_point := type_vector_model (set (
					x => w - b,
					y => y));

				-- right end:
				l.end_point := type_vector_model (set (
					x => w,
					y => y));
				
				draw_line;
			end loop;

			stroke;

			
			
			-- COLUMN INDEX:
			y := b / 2;

			-- x requires offset to the right
			xo := b - (sector_width / 2);
			
			for i in 1 .. frame.sectors.columns loop

				-- compute x coordinate
				x := pac_geometry.type_distance_positive (i) * sector_width + xo;
				
				-- draw index in lower border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (x, y)));

				-- draw index in upper border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (
								x => x,
								y => h - y)));
				
			end loop;

			
			-- ROW INDEX:
			x := b / 2;

			-- y requires offset upwards
			yo := b - (sector_height / 2);
			
			for i in 1 .. frame.sectors.rows loop

				-- compute y coordinate
				y := pac_geometry.type_distance_positive (i) * sector_height + yo;
				
				-- draw index in left border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (x, y)));

				-- draw index in right border
				draw_index (
					content	=> to_content (to_string (i)),
					pos		=> type_vector_model (set (
								x => w - x,
								y => y)));
				
			end loop;
			
		end sector_delimiters;

		
		
	begin
		outer_border;

		inner_border;

		sector_delimiters;
	end draw_frame;



	

	procedure draw_common_placeholders (
		placeholders			: in type_placeholders_common;
		title_block_position	: in pac_geometry.type_position)
	is
		use et_text;
		use pac_draw_text;
		use et_project;
		use et_project.modules;
		use et_assembly_variants;

		-- A temporarily storage place for the
		-- position of a placeholder:
		pos : type_vector_model;
		
	begin
		-- PROJECT NAME:		
		pos := to_vector (placeholders.project_name.position);

		draw_text (
			content		=> to_content (to_string (active_project)),					  
			size		=> to_distance (placeholders.project_name.size),
			font		=> font_placeholders,

			-- The anchor point is offset by the position of the title block:
			anchor		=> add (pos, title_block_position.place),
			
			origin		=> false,
			rotation	=> 0.0,
			alignment	=> (LEFT, BOTTOM));


		
		-- MODULE FILE NAME:		
		pos := to_vector (placeholders.module_file_name.position);

		draw_text (
			content		=> to_content (get_active_module),
			size		=> to_distance (placeholders.module_file_name.size),
			font		=> font_placeholders,

			-- The anchor point is offset by the position of the title block:
			anchor		=> add (pos, title_block_position.place),
			
			origin		=> false,
			rotation	=> 0.0,
			alignment	=> (LEFT, BOTTOM));



		-- ASSEMBLY VARIANT:		
		pos := to_vector (placeholders.active_assembly_variant.position);

		draw_text (
			--content		=> to_content (to_variant (element (active_module).active_variant)),
			content		=> to_content ("to do"), -- CS
			size		=> to_distance (placeholders.active_assembly_variant.size),
			font		=> font_placeholders,

			-- The anchor point is offset by the position of the title block:
			anchor		=> add (pos, title_block_position.place),
			
			origin		=> false,
			rotation	=> 0.0,
			alignment	=> (LEFT, BOTTOM));

		
	end draw_common_placeholders;



	procedure draw_static_texts (
		texts					: in pac_static_texts.list;
		title_block_position	: in pac_geometry.type_position)
	is
		use et_text;
		use pac_draw_text;
		
		-- A temporarily storage place for the
		-- position of a text:
		pos : type_vector_model;

		use pac_static_texts;

		
		procedure query_text (c : in pac_static_texts.cursor) is
			text : type_static_text renames element (c);
		begin
			pos := to_vector (text.position);
			
			draw_text (
				content		=> text.content,
				size		=> to_distance (text.size),
				font		=> font_placeholders,

				-- The anchor point is offset by the position of the title block:
				anchor		=> add (pos, title_block_position.place),
				
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (LEFT, BOTTOM));
		end query_text;
		
	begin
		iterate (texts, query_text'access);
	end draw_static_texts;



	procedure draw_basic_meta_information (
		meta					: in et_meta.type_basic;
		title_block_position	: in pac_geometry.type_position)
	is
	begin

		null;
	end draw_basic_meta_information;


	
end et_canvas.drawing_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
