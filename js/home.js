// Main entry point
$(function(){
  //setupVids();
  //setupFeatures();

  $('.features .col-md-6').click(function(){
    $(this).find('.collapse').collapse('toggle');
  });
});

// Setup hovering of video thumbnails
function setupVids(){
  var $community = $('.community');
  var $videos = $('.videos');
  var $tagline = $('#tagline');
  var $videoDesc = $('#video-description');
  var $videoAnchor = $('#video-anchor');
  var $videoView = $('#video-view');
  var originalBackground = $community.css('background');

  $videoDesc.hide();
  // To keep a consistent height between transitions
  $videoDesc.css('height',$tagline.height());

  $('.vid-thumbnail').each(function(){
    var $this = $(this);
    var title = $this.attr('title');
    var href = $this.attr('href');
    $this.click(select);
    function select(){
      $videos.find('.current').removeClass('current');
      $this.addClass('current');
      $videoAnchor.text(title);
      $videoAnchor.attr('href',href);
      $videoView.attr('href',href);
      $tagline.hide();
      $videoDesc.show();
      $community.css('background','#111111');
      return false;
    }
  });
}

// Expandable features
function setupFeatures(){
  $('.features .span6').each(function(){
    var $this = $(this);
    $this.click(function(){
      $this.find('.expandable').slideToggle(function(){
        $this.find('.expand').slideToggle('fast');
      });
    });
    if ($this.find('.expandable').size() == 0)
      $this.find('.expand').hide();
  });
}

