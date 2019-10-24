#version 330

// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.


// This shader computes the distance to the Mandelbrot Set for everypixel, and colorizes
// it accoringly.
// 
// Z -> Z²+c, Z0 = 0. 
// therefore Z' -> 2·Z·Z' + 1
//
// The Hubbard-Douady potential G(c) is G(c) = log Z/2^n
// G'(c) = Z'/Z/2^n
//
// So the distance is |G(c)|/|G'(c)| = |Z|·log|Z|/|Z'|
//
// More info here: http://www.iquilezles.org/www/articles/distancefractals/distancefractals.htm

in  vec2 fragCoord;
uniform  int time;
out vec4 fragColor;

void main()
{
  int step = 20;

  vec3  iResolution = vec3(1024, 1024, 1.0);
  float iGlobalTime = time / 30.3;
  vec2  p           = -3.0 + 5000.0 * fragCoord.xy / iResolution.xy;
  p.x              *= iResolution.x/iResolution.y;

  // animation	
	float tz = 0.5 - 0.5*cos(0.225*iGlobalTime);
  float zoo = pow( 0.5, 13.0*tz );
	vec2 c = vec2(-0.05,.6805) + p*zoo;

  // iterate
  vec2 z  = vec2(0.0);
    float mz = 0.0;
    int iter = 0;
  for( int i=0; i<256; i++ )
    {
			
      // Z -> Z² + c			
      z = vec2( z.x*z.x - z.y*z.y, 2.0*z.x*z.y ) + c;
        mz = sqrt(z.x*z.x + z.y*z.y);
        if(mz>2.0) {
            iter = i;
            break;
        }else{
            continue;
        }
    }

	
  // do some soft coloring based on distance
	//d = clamp( 8.0*d/zoo, 0.0, 1.0 );
	//d = pow( d, 0.25 );
  float cc = float (iter*2);
  //double dcol = sqrt(cc*cc + iter*iter + iter*iter);
  float nc = cc/600.0f;
  float niter = iter/600.0f;
  float colx = niter;
    
  // fragColor = vec4( vec3(fragCoord.x,fragCoord.y,0.0), 1.0 );
  
  iGlobalTime = (time+step) / 30.3;
  p           = -3.0 + 5000.0 * fragCoord.xy / iResolution.xy;
  p.x              *= iResolution.x/iResolution.y;

  // animation	
	tz = 0.5 - 0.5*cos(0.225*iGlobalTime);
  zoo = pow( 0.5, 13.0*tz );
	c = vec2(-0.05,.6805) + p*zoo;

  // iterate
  z  = vec2(0.0);
  mz = 0.0;
  iter = 0;
  for( int i=0; i<256; i++ )
    {
			
      // Z -> Z² + c			
      z = vec2( z.x*z.x - z.y*z.y, 2.0*z.x*z.y ) + c;
        mz = sqrt(z.x*z.x + z.y*z.y);
        if(mz>2.0) {
            iter = i;
            break;
        }else{
            continue;
        }
    }

	
  cc = float (iter*2);
  nc = cc/600.0f;
  niter = iter/600.0f;
  float coly = niter;

  
 iGlobalTime = (time+step+step) / 30.3;
  p           = -3.0 + 5000.0 * fragCoord.xy / iResolution.xy;
  p.x              *= iResolution.x/iResolution.y;

  // animation	
	tz = 0.5 - 0.5*cos(0.225*iGlobalTime);
  zoo = pow( 0.5, 13.0*tz );
	c = vec2(-0.05,.6805) + p*zoo;

  // iterate
  z  = vec2(0.0);
  mz = 0.0;
 iter = 0;
  for( int i=0; i<256; i++ )
    {
			
      // Z -> Z² + c			
      z = vec2( z.x*z.x - z.y*z.y, 2.0*z.x*z.y ) + c;
        mz = sqrt(z.x*z.x + z.y*z.y);
        if(mz>2.0) {
            iter = i;
            break;
        }else{
            continue;
        }
    }

	
  cc = float (iter*2);
  nc = cc/600.0f;
  niter = iter/600.0f;
  float colz = niter;


  vec3 col = vec3(colx, coly, colz);
  fragColor = vec4(col, 1.0);
}
