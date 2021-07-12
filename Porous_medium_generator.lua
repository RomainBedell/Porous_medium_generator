-- The MIT License
-- Copyright © 2021 Romain Bedell
-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

		--Function use to generate different shape
		function gen_shape(xy, z, color, p_size, p_width, p_t, p_t_x, p_t_y, s)

		if s == 1 then
		-- shape 1 : trabecular

		--implicit shape generation
		  ini_shape = implicit_distance_field(v(-xy,-xy,0),v(xy,xy,z),[[

		  vec3 hash33(vec3 c, float r) {
		      vec3 h = .5*(fract(vec3(8., 1., 64.)*sin( dot(vec3(17., 59.4, 15.), c) )*32768.)-.5);
		      return mix(vec3(.4), h, r); // attenuate randomness
		  //  return h; // simple version
		  }

		  /* 3d simplex noise from candycat's "Noise Lab (3D)" https://www.shadertoy.com/view/4sc3z2
		  based on the one by nikat: https://www.shadertoy.com/view/XsX3zB */
		  vec4 simplex_noise(vec3 p, float r) {
		      
		      const float K1 = .333333333;
		      const float K2 = .166666667;
		      
		      vec3 i = floor(p + (p.x + p.y + p.z) * K1);
		      vec3 d0 = p - (i - (i.x + i.y + i.z) * K2);
		      
		      vec3 e = step(vec3(0.), d0 - d0.yzx);
		      vec3 i1 = e * (1. - e.zxy);
		      vec3 i2 = 1. - e.zxy * (1. - e);
		      
		      vec3 d1 = d0 - (i1 - 1. * K2);
		      vec3 d2 = d0 - (i2 - 2. * K2);
		      vec3 d3 = d0 - (1. - 3. * K2);
		      
		      vec4 h = max(.6 - vec4(dot(d0, d0), dot(d1, d1), dot(d2, d2), dot(d3, d3)), 0.);

		      vec4 n = h * h * h * h * vec4(dot(d0, hash33(i, r)), dot(d1, hash33(i + i1, r)), dot(d2, hash33(i + i2, r)), dot(d3, hash33(i + 1., r)));
		      
		      return 100.*n; 
		  }

		  // see https://www.shadertoy.com/view/ttsyRB
		  vec4 variations(vec4 n) {
		      vec4 an = abs(n);
		      vec4 s = vec4(
		          dot( n, vec4(1.) ),
		          dot( an,vec4(1.) ),
		          length(n),
		          max(max(max(an.x, an.y), an.z), an.w) );
		      
		      float t =.27;
		      
		      return vec4(
		          // worms
		          max(0., 1.25*( s.y*t-abs(s.x) )/t),
		          // cells (trabeculae)
		          pow( (1.+t)*( (1.-t)+(s.y-s.w/t)*t), 1.), 
		//step( .7, (1.+t)*( (1.-t)+(s.y-s./t)*t) ), //legacy 
		          .75*s.y,
		          .5+.5*s.x);
		  }

		float progress;
		float compact;

		  float distance(vec3 p) {
		    float c = smoothstep(0., 1., length(p.xyz)); // controls the randomness
		    p += vec3(-.65, .35, 44.85);

		 	float s = ]] .. p_size .. [[;
			float pw = ]] .. p_width .. [[;
			progress = s;
			//progress = s*p.z/100; //progress size base on p.z
			compact = pw;
			//compact = pw*(1-(p.z*1.3)/1000); // pore w base on p.z

		    float n = variations( simplex_noise(p*progress*.5, c) ).y;
			n = compact-n;

			n /= progress*4.;
		      
		    return n;

		  }
		  ]])

		elseif s == 2 then
		--shape 2 columnar cyl base

		--number of cyl
		nc = ui_number("Number of Cylinder",40,1,200)
		-- z size of each cyl (to add some control randomness contact me)
		z_cyl = ui_number("z_cyl",5,0,100)

		--implicit shape generation
		ini_shape = translate(0,0,10)*rotate(0,90,0)*implicit_distance_field(v(-xy,-xy,-z),v(xy,xy,z),[[
		vec3 a = vec3(]] .. z_cyl .. [[,0,0);
		vec3 b =vec3(-]] .. z_cyl .. [[,0,0);
		float r = ]] .. xy .. [[/10;

		uniform float alterate = 0.;

		float distance(vec3 p)
		{
		    vec3 pa = p - a;
		    vec3 ba = b - a;
		    float baba = dot(ba,ba);
		    float paba = dot(pa,ba);

		    float x = length(pa*baba-ba*paba) - r*baba;
		    float y = abs(paba-baba*0.5)-baba*0.5;
		    float x2 = x*x;
		    float y2 = y*y*baba;
		    float d = (max(x,y)<0.0)?-min(x2,y2):(((x>0.0)?x2:0.0)+((y>0.0)?y2:0.0));
		    return sign(d)*sqrt(abs(d))/baba+noise(p*alterate);
		}

		]])
		--enable_progressive_rendering(ini_shape,false)

		cuttingbox = cube(xy,xy,z)


		--determine seed use to randomise the position of cyl
		seed = ui_number("Seed",1,0,100)

		math.randomseed(seed) 

		--itarate base of the number of cyl put in nc "number cyl"
		for i = 1,nc do

		set_uniform_scalar(ini_shape, 'alterate', math.random(20,40)*0.01)
		-- randomly distribute cyl poistion over a box space
		collo = translate(math.random(-15,15),math.random(-15,15),math.random(0,z))*rotate(0,0,0)*ini_shape
		--merge every cyl into one shape
		fshape = union{translate(p_t_x,p_t_y,p_t)*intersection(collo,ccylinder(xy/2,z)),fshape}

		end

		return fshape

		elseif s == 3 then
		--shape 3 columnar hex base

		-- number of cyl
		nc = ui_number("Number of Cylinder",40,1,200)
		-- z size of each cyl (to add some control randomness contact me)
		z_cyl = ui_number("z_cyl",5,0,100)

		--implicit shape generation
		  ini_shape = implicit_distance_field(v(-xy,-xy,0),v(xy,xy,z),[[
		vec2 h = vec2(]] .. xy .. [[/10,]] .. z_cyl .. [[/2);
		uniform float alterate = 0.;
		float distance( vec3 p )
		{
		    vec3 q = abs(p);

		    const vec3 k = vec3(-0.8660254, 0.5, 0.57735);
		    p = abs(p);
		    p.xy -= 2.0*min(dot(k.xy, p.xy), 0.0)*k.xy;
		    vec2 d = vec2(
		       length(p.xy - vec2(clamp(p.x, -k.z*h.x, k.z*h.x), h.x))*sign(p.y - h.x),
		       p.z-h.y );
		    return min(max(d.x,d.y),0.0) + length(max(d,0.0))+noise(p*alterate)+0.5;
		}
		]])

		cuttingbox = cube(xy,xy,z)

		seed = ui_number("Seed",1,0,100)

		math.randomseed(seed) 

		for i = 1,nc do

		set_uniform_scalar(ini_shape, 'alterate', math.random(10,50)*0.01)

		collo = translate(math.random(-15,15),math.random(-15,15),math.random(0,z))*rotate(0,0,0)*ini_shape

		fshape = union{translate(p_t_x,p_t_y,p_t)*intersection(collo,ccylinder(xy/2,z)),fshape}

		end

		return fshape

		end

		  --permit progressive rendering or not, comment if low PC spec
		  enable_progressive_rendering(ini_shape,false)

		  --cleanup implicit offset
		  cleanup_cube = cube(xy,xy,z)

		  my_fill = intersection{translate(0,0,0)*ini_shape,
		cleanup_cube
		} 

		  --limiting shape size to a cyl
		  cutting_box_cylinder =  ccylinder(xy/2,z*2)

		  --cleaning cube use to fixe rare offset du to artefact
		  cleaned_box = intersection{
		    my_fill,
		   cleanup_cube
		  }

		  my_f_form = translate(0,0,0)*intersection{translate(0,0,0)*cleaned_box,
		cutting_box_cylinder
		}

		  return my_f_form

		end

		--list for Root selection
		choice_list = {
		{0,"No"},
		{1,"Yes"},
		}

		add_root = ui_radio("Root_", choice_list)

		-- define number of different shape you want
		n = ui_number("Additional Form",1,1,10)

		for i = 1, n do

		-- choose shape settings

		shape_list = {

		{1,"Form_"..i},
		{2,"Hollow_"..i},
		{3,"Hollow & Form Merge_"..i}
		}

		shape = ui_radio("Shape_Form_"..i, shape_list)

		-- Which shape do you want beetween the three possible : Trabecular, cyl, hex

		s = ui_number("Shape_"..i,1,1,3)

		--xy size
		xy = ui_number("Diameter_"..i,40,0,100)
		--z size
		z = ui_number("Height_"..i,70,0,200)
		-- color for model visibility and brush
		color = ui_scalarBox("Color_"..i,6,1)
		--granule size 
		p_size = ui_scalarBox("Grain Number_"..i,0.1,0.1)
		--pore width
		p_width = ui_scalarBox("Infill_"..i,0.8,0.1)

		--shape translation
		p_t_x = ui_number("Form Translate x_"..i,0,-100,100)
		p_t_y = ui_number("Form Translate Y_"..i,0,-100,100)
		p_t = ui_number("Form Translate Z_"..i,0,0,100)
		-- trabecular shape
		f_shape = gen_shape(xy, z, color, p_size, p_width, p_t, p_t_x, p_t_y, s)

		if add_root == 1 then 
		--settings for root system
		show_root = ui_number("Show Root",0,0,1)
		root_file = ui_text("Root file","D:\\root_exemple.stl")
		r_scale = ui_number("Root Scaling",1,0,100)

		r_t_x = ui_number("Root Translate x",0,-100,100)
		r_t_y = ui_number("Root Translate Y",0,-100,100)
		r_t_z = ui_number("Root Translate Z",0,-100,100)

		r_r_x = ui_number("Root Rotate x",0,0,360)
		r_r_y = ui_number("Root Rotate Y",0,0,360)
		r_r_z = ui_number("Root Rotate Z",0,0,360)

		root_f = scale(r_scale)*rotate(r_r_x,r_r_y,r_r_z)*translate(r_t_x,r_t_y,r_t_z)*load_centered_on_plate(root_file)

		if show_root == 1 then

		emit(root_f)

		end

		if shape == 1 then

		f_shape = union(f_shape,root_f)

		elseif shape == 2 then

		f_shape = difference(f_shape,root_f)

		end

		else

		end
				  --cleaning cube use to fixe rare offset du to artefact
		box = difference{
		cleanup_cube,
		f_shape}

		form = intersection{translate(0,0,0)*box,cutting_box_cylinder
		}
		-- end shape def

		-- if shape form is choose
		if shape == 1 then

		--render shape
		--form = difference{form,imp_asset0,cutting_center}
		emit(scale(1)*translate(p_t_x,p_t_y,p_t)*form,color)

		--if shape hollow is choose
		elseif shape == 2 then

		--render shape 
		shape_list = {
		{0,"Yes_"..i},
		{1,"No_"..i},
		}

		show = ui_radio("Shape_"..i, shape_list)

		if i == 0 then

		if show == 0 then

		emit(translate(p_t_x,p_t_y,p_t)*f_shape,color)

		end

		p_shape = f_shape

		else

		if show == 0 then

		emit(translate(p_t_x,p_t_y,p_t)*intersection{p_shape,f_shape},color)

		end

		p_shape = f_shape

		end

		elseif shape == 3 then

		--render both
		emit(form,color)
		shape_list = {
		{0,"Yes_"..i},
		{1,"No_"..i},
		}

		show = ui_radio("Shape_"..i, shape_list)

		if i == 0 then

		if show == 0 then
		emit(f_shape,3)
		end
		p_shape = f_shape
		else
		if show == 0 then
		emit(translate(0,0,0)*intersection{p_shape,f_shape},3)
		end
		p_shape = f_shape
		end
		-- end for shape choosing
		end
		-- end for layer number loop
		end
